
BEGIN {
    buffer=""
    version=substr(ENVIRON["GITHUB_REF_NAME"], 2)
}

/^## / {
    if (index($0, version)) {
        in_version=1
    } else if (in_version) {
        exit # at the next section
    }
}

!in_version { next }

# start at the first sub section of the version section
/^### / {
    print_out=1
}

!print_out { next }

# buffer empty lines until hitting text again; cuts off empty lines at the end
/^[ \t]*$/ {
    buffer=buffer $0 "\n"
    next
}

# print lines and buffer
{
    print buffer $0
    buffer=""
}
