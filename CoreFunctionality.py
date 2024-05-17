import Repository_loader as rl
import re
import os
from langchain_openai import ChatOpenAI
from langchain_core.prompts.chat import (
    ChatPromptTemplate,
    HumanMessagePromptTemplate,
    SystemMessagePromptTemplate,
)
from getpass import getpass


OPENAI_API_KEY = getpass("Enter your OpenAI API key: ")
os.environ["OPENAI_API_KEY"] = OPENAI_API_KEY

# Calls method to return branches and local repo path. Clones repo first if a Git url is inputted. 
def return_branches(_path: str): 
    pattern = r'^https'
    if re.match(pattern, _path):
        return rl.get_branches_online(_path)
    else:
        return rl.get_branches_local(_path)

# sets up global variables for the vector database, embeddings model and llm
def setup(_repo_path: str, _branch: str, _temperature: float):
    global vector_database
    global embeddings_model
    global llm

    vector_database, embeddings_model = rl.load_repo(_repo_path, _branch)
    llm = ChatOpenAI(model="gpt-4-turbo", temperature=_temperature)



# query model and return answer based on context
def query_model(_query: str, _testing: bool):
    if(vector_database is None or embeddings_model is None or llm is None):
        return "Please load a repository and model first"
    
    embedded_query = embeddings_model.embed_query(_query)
    context = vector_database.similarity_search_by_vector(embedded_query, k=32)

    if(not _testing):
        index = 0
        while index < len(context):
            if "test" in context[index].metadata.get("file_path"):
                context.pop(index)
            else:
                index += 1


    template = "You are an AI programming assistant. You give comprehensive answers about software projects based on pieces of its source code. Use the following pieces of context to answer the question at the end:\n{context}"
    system_message_prompt = SystemMessagePromptTemplate.from_template(template)
    human_template = "{question}"
    human_message_prompt = HumanMessagePromptTemplate.from_template(human_template)
    chat_prompt = ChatPromptTemplate.from_messages(
        [system_message_prompt, human_message_prompt]
    )

    response = llm.invoke(
        chat_prompt.format_prompt(
            context=context, question=_query
        ).to_string()
    )

    rl.log_data(_query=_query, _response=response.content)
   
    return response.content