import sys
import requests

for artid in sys.argv[1:]:
    req = requests.post("https://api.serlo.org/graphql", json={"query": """query($id: Int!) {
        uuid(id: $id) {
            ... on Article {
                currentRevision {
                    title
                    content
                }
            }
        }}""", "variables": {"id": int(artid)}})

    if req.status_code == 200:
        print(req.text)
    else:
        print(req.status_code)
        print(req.text)
        sys.exit(1)
