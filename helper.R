remove.packages(c("curl", "digest", "jsonlite"))

renv::install(c("curl", "digest", "jsonlite"))


install.packages(c("curl", "digest", "jsonlite"))


packageVersion("curl")    # Should be >= 5.1.0
packageVersion("digest")  # Should be >= 0.6.33
packageVersion("jsonlite")# Should be >= 1.8.7



list.files()  # Check the files in the current directory


# Verify the updated versions
print(packageVersion("curl"))
print(packageVersion("digest"))
print(packageVersion("jsonlite"))


# Deploy the app
rsconnect::deployApp('/Users/student/Documents/cfb bayes model')


rsconnect::setAccountInfo(name='jriordan55', token='56A510C1ABDC172524E2CE54711E778A', secret='SsaLePftRj/sJzYW+9lB4iLokw2beRdhfePlN9Xq')

# Initialize renv if not already done
renv::init()
