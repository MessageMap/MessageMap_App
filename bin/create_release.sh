# Build Code
./rebar3 release

# Start RPM BUILD PROCESS
mkdir -p rel/usr/messagemap
mkdir -p rel/etc/systemctl/system
mkdir -p rel/var/messageMap
mkdir -p rel/tmp/messageMap
mkdir -p rel/etc/messagemap
mkdir -p rel/var/log/messagemap

rsync -avz _build/default/ rel/usr/messagemap/
cp installer_files/messagemap.service rel/etc/systemctl/system/
cp installer_files/messagemap.crt rel/etc/messagemap/
cp installer_files/messagemap.key rel/etc/messagemap/
cp installer_files/messagemap.conf rel/etc/messagemap/

# Add requirement erlang
/home/ethan/.gem/ruby/3.0.0/bin/fpm -s dir \
    -t deb \
    -n messagemap \
    -C "$(pwd)/rel" \
    -v 0.0.1 \
    --iteration 1 \
    -m Ben@MessageMap.io \
    --description "MessageMap - Smart Message Queue Service"

sudo mv *.deb packages/
sudo docker run --rm -v "$(pwd)/packages":/tmp debian:stable sh -c 'dpkg-deb -c /tmp/messagemap_*.deb'
sudo docker run --rm -v "$(pwd)/packages":/tmp erlang:23 sh -c 'dpkg -i /tmp/messagemap_*.deb && ls -l /usr/messagemap/'

/home/ethan/.gem/ruby/3.0.0/bin/fpm -s dir \
    -t rpm \
    -n messagemap \
    -C "$(pwd)/rel" \
    -v 0.0.1 \
    --iteration 1 \
    -m Ben@MessageMap.io \
    --description "MessageMap - Smart Message Queue Service"
mv *.rpm packages/

