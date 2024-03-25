import Repository_loader as rl
import re






def return_branches(_path): 
    pattern = r'^https'
    if re.match(pattern, _path):
        return rl.get_branches_online(_path)
    else:
        return rl.get_branches_local(_path)
