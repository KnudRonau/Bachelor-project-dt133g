import git
import re
import os
from langchain_community.document_loaders.git import GitLoader
from langchain_openai import OpenAIEmbeddings
from langchain_community.vectorstores.faiss import FAISS
from langchain.text_splitter import RecursiveCharacterTextSplitter


BASE_PATH = "./repo_data/"
BASE_VECTORDATABASE_PATH = "./vector_database_data/"
REPO_NAME_PATTERN = r'[^/]+$'

# clones repo and returns local path along with list of branches
def get_branches_online(_path: str):
    local_repo_path = BASE_PATH + re.search(REPO_NAME_PATTERN, _path).group(0)
    if os.path.exists(local_repo_path):
        return get_branches_local(local_repo_path)
    repo = git.Repo.clone_from(url = _path, to_path = local_repo_path)
    branches = [str(branch) for branch in repo.branches]
    repo.close()
    return branches, local_repo_path

# returns path and branches
def get_branches_local(_path: str):
    repo = git.Repo(_path)
    branches = [str(branch) for branch in repo.branches]
    repo.close()
    return branches, _path

# splits repo and embeds it and stores it in a vector database. Returns DB and embedding model. 
def load_repo(_path: str, _branch: str):
    loader = GitLoader(
        repo_path=_path,
        branch=_branch
    )

    repo = loader.load()

    vector_database_path = BASE_VECTORDATABASE_PATH + re.search(REPO_NAME_PATTERN, _path).group(0)

    embeddings_model = OpenAIEmbeddings(model="text-embedding-3-large")

    if os.path.exists(vector_database_path):
        db = FAISS.load_local(folder_path=vector_database_path, embeddings=embeddings_model, allow_dangerous_deserialization=True)
        print("Loaded existing vector database")
        return db, embeddings_model

    coding_separators = [
        "\nenum ",
        "\ninterface ",
        "\nnamespace ",
        "\nimplements ",
        "\ndelegate ",
        "\nevent ",
        # Split along class definitions
        "\nclass ",
        "\ndef ",
        "\n\tdef ",
        "\nobject ",
        "\nstruct ",
        "\nabstract ",
        # Split along function definitions
        "\nvoid ",
        "\nint ",
        "\nfloat ",
        "\ndouble ",
        "\nfunc ",
        "\nvar ",
        "\nconst ",
        "\ntype ",
        "\npublic ",
        "\nprotected ",
        "\nprivate ",
        "\nstatic ",
        "\ninternal ",
        "\ncompanion ",
        "\nfun ",
        "\nval ",
        "\nfunction ",
        "\nlet ",
        "\nfn ",
        "\nreturn ",
        # Split along control flow statements
        "\nif ",
        "\nfor ",
        "\ndo ",
        "\nwhile ",
        "\nswitch ",
        "\ncase ",
        "\nwhen ",
        "\nelse ",
        "\ndefault ",
        "\nbegin ",
        "\nrescue ",
        "\nunless ",
        "\nloop ",
        "\nmatch ",
        "\ncontinue ",
        "\nforeach ",
        "\nbreak ",
        "\ndo while ",
        "\nassembly ",
        # Split by exceptions
        "\ntry ",
        "\nthrow ",
        "\nfinally ",
        "\ncatch ",
        # Split by the normal type of lines
        "\n\n",
        "\n",
        " ",
        "",
        # PROTO
        "\nmessage ",
        "\nimport ",
        "\nservice ",
        "\nsyntax ",
        "\noption ",
        # RST
        "\n=+\n",
        "\n-+\n",
        "\n\\*+\n",
        "\n\n.. *\n\n",
        # markdown
        "\n#{1,6} ",
        "```\n",
        "\n\\*\\*\\*+\n",
        "\n---+\n",
        "\n___+\n",
        # html
        "<body",
        "<div",
        "<p",
        "<br",
        "<li",
        "<h1",
        "<h2",
        "<h3",
        "<h4",
        "<h5",
        "<h6",
        "<span",
        "<table",
        "<tr",
        "<td",
        "<th",
        "<ul",
        "<ol",
        "<header",
        "<footer",
        "<nav",
        # Head
        "<head",
        "<style",
        "<script",
        "<meta",
        "<title",
        "",
        # sol
        "\npragma ",
        "\nusing ",
        "\ncontract ",
        "\nlibrary ",
        "\nconstructor ",
        #cobol
        "\nIDENTIFICATION DIVISION.",
        "\nENVIRONMENT DIVISION.",
        "\nDATA DIVISION.",
        "\nPROCEDURE DIVISION.",
        "\nWORKING-STORAGE SECTION.",
        "\nLINKAGE SECTION.",
        "\nFILE SECTION.",
        "\nINPUT-OUTPUT SECTION.",
        "\nOPEN ",
        "\nCLOSE ",
        "\nREAD ",
        "\nWRITE ",
        "\nIF ",
        "\nELSE ",
        "\nMOVE ",
        "\nPERFORM ",
        "\nUNTIL ",
        "\nVARYING ",
        "\nACCEPT ",
        "\nDISPLAY ",
        "\nSTOP RUN.",
    ]

    text_splitter = RecursiveCharacterTextSplitter(
        chunk_size=1600,
        chunk_overlap=200,
        separators=coding_separators
    )

    split_repo = text_splitter.split_documents(repo)

    for document in split_repo:
        document.metadata.pop("source", None)

    db = FAISS.from_documents(documents=split_repo, embedding=embeddings_model)

    db.save_local(folder_path=vector_database_path)
    print("Saved vector database to local storage")

    return db, embeddings_model

