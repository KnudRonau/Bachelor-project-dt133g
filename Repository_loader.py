import git
import re
import os

BASE_PATH = "./repo_data/"

def get_branches_online(_path):
    repo_name_pattern = r'[^/]+$'
    local_repo_path = BASE_PATH + re.search(repo_name_pattern, _path).group(0)
    if os.path.exists(local_repo_path):
        return get_branches_local(local_repo_path)
    repo = git.Repo.clone_from(url = _path, to_path = local_repo_path)
    branches = [str(branch) for branch in repo.branches]
    repo.close()
    return branches, local_repo_path

def get_branches_local(_path):
    repo = git.Repo(_path)
    branches = [str(branch) for branch in repo.branches]
    repo.close()
    return branches, _path


