# Build Code
./rebar3 release

# Start RPM BUILD PROCESS
mkdir -p rel/usr/messagemap
mkdir -p rel/etc/systemd/system
mkdir -p rel/var/messageMap
mkdir -p rel/tmp/messageMap
mkdir -p rel/etc/messagemap
mkdir -p rel/var/log/messagemap

rsync --delete -avz --exclude='.git/' --exclude='.buid-id/' _build/default/ rel/usr/messagemap/
#cp -r _build/default/rel/messagemap/erts* rel/usr/messagemap/
# Clean up
find ./rel -iname ".git" | xargs rm -rf
find ./rel -iname "test" | xargs rm -rf

cp installer_files/messagemap.service rel/etc/systemd/system/
# Update to generate ssl files on build
cp installer_files/messagemap.crt rel/etc/messagemap/
cp installer_files/messagemap.key rel/etc/messagemap/
cp installer_files/messagemap.conf rel/etc/messagemap/

# Add requirement erlang
/home/ethan/.gem/ruby/3.0.0/bin/fpm -s dir \
    -t deb \
    -n messagemap \
    -C "$(pwd)/rel" \
    -v 0.1.0 \
    --depends erlang \
    --iteration 1 \
    -m Ben@MessageMap.io \
    --description "MessageMap - Smart Message Queue Service"

sudo mv *.deb packages/
#sudo docker run --rm -v "$(pwd)/packages":/tmp debian:stable sh -c 'dpkg-deb -c /tmp/messagemap_*.deb'
#sudo docker run --rm -v "$(pwd)/packages":/tmp erlang:23 sh -c 'dpkg -i /tmp/messagemap_*.deb && ls -l /usr/messagemap/'

/home/ethan/.gem/ruby/3.0.0/bin/fpm -s dir \
    -t rpm \
    --depends erlang \
    -n messagemap \
    -C "$(pwd)/rel" \
    -v 0.1.0 \
    --iteration 1 \
    -m Ben@MessageMap.io \
    --rpm-rpmbuild-define "_build_id_links none" \
    --description "MessageMap - Smart Message Queue Service"
mv *.rpm packages/

scp -P 122 -r packages/* root@169.59.15.181:~/packages/

#Size
#>du -sh packages/*
#62M	packages/messagemap_0.0.1-1_amd64.deb
#62M	packages/messagemap-0.0.1-1.x86_64.rpm
