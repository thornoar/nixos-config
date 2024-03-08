echo -e "\e[35m| Push All Git Repositories |\e[0m"
basewd=$PWD
cd $PROJECTS
for dir in */
do
    echo ""
    echo -e "\e[34m> Entering $dir...\e[0m"
    cd $dir
    if [ -d .git ]; then
        git add . && git commit -m "--" && git push
    else
        echo -e "\e[33mNot a git repository, skipping...\e[0m"
    fi
    cd ..
done
cd "$basewd"
