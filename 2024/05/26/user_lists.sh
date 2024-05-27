gh api graphql --paginate \
    -F login=jmikedupont2 \
    -f query="`cat user_lists.graphql`"
