# Oangou

The code source is now hosted on [Codeberg](https://codeberg.org/EruEri/oangou)

A program to encrypt data, send it over an channel, and decrypt it on the other side.

`oangou` works by using public and private keys and the **Diffie–Hellman** key-exchange protocol.


## Installation

First you will need to install those opam packages.
```sh
$ opam install dune xdg yojson cmdliner ppx_deriving_yojson mirage_crypto_ec
```

By default the prefix install is `/usr/local`. So oangou binary is installed in `/usr/local/bin` and the man pages in `/usr/local/share/man`. 
But the `make install` rule reacts to 3 variables:
- `PREFIX`: 
  - default: `/usr/local`
- `BINDIR`: 
    - default: `$(PREFIX)/bin`
- `MANDIR`: 
    - default: `$(PREFIX)/share/man`

```sh
$ git clone https://github.com/EruEri/oangou
$ cd oangou
$ make 
$ make install 
```

## Initialization

To start with oangou, you first need to initialize it

```
$ oangou init --help
NAME
       oangou-init - Initialize oangou

SYNOPSIS
       oangou init [--force] [OPTION]…

DESCRIPTION
       Initialize oangou by creating XDG_DATA_HOME/oangou/.oangourc file

       If oangou has already been initialized, oangou init will raise an
       exception unless the --force option is given which will delete the
       existing oangou installation

OPTIONS
       -f, --force
           Force the initialisation
```

## Add

To add a peer to oangou, you need to get it public key. Once you have it, use `oangou-add`
```
$ oangou add --help
NAME
       oangou-add - Add peers

SYNOPSIS
       oangou add [-k <PUBLIC_KEY>] [-p <PEER>] [-x] [OPTION]…

OPTIONS
       -k <PUBLIC_KEY> (absent=stdin)
           Associate <PUBLIC_KEY> to <PEER>

       -p <PEER> (required)
           Add <PEER> to known peers

       -x  Treat the input key as a hexadecimal string
```

Once a peer is added, a secret is generated by combining your private key with the peer's public key based on the Elliptic curve Diffie–Hellman.

The peer will also need of your public key to decrypt your message.

## Export

To export keys from `oangou` use `oangou export keys` command.

## Encrypt

Once you have the public key targeted peer, you can encrypt a message from stdin or from a file using `oangou encrypt`

```
$ oangou encrypt --help
NAME
       oangou-encrypt - Encrypt data

SYNOPSIS
       oangou encrypt [OPTION]…

OPTIONS
       -f <FILE> (absent=stdin)
           Encrypt a specific file

       -o <OUTFILE> (absent=stdout)
           Output the encrypt file to <OUTFILE>

       -p <PEER> (required)
           Encrypt the file for <PEER>

       -x  Output as a hexadecimal string
```

## Decrypt

After receiving the receiving the encrypted message or file, you can decrypt it using `oangou decrypt`

```
NAME
       oangou-decrypt - Decrypt data

SYNOPSIS
       oangou decrypt [OPTION]…

OPTIONS
       -f <FILE> (absent=stdin)
           Decrypt a specific file

       -o <OUTFILE> (absent=stdout)
           Output the encrypt file to <OUTFILE>

       -p <PEER> (required)
           Decrypt the file for <PEER>

       -x  Treat input as a hexadecimal string
```

## Exemple

Alice wants to send the message "Hello bob from alice" to Bob.

This exemple assumes that Alice and Bob know the public key of the other one. 

```
# Alice 

$ echo "Hello bob from alice" | oangou encrypt -x -p bob
<an_hexadecimal_string>
```

```
# Bob

$ echo "<an_hexadecimal_string>" | oangou decrypt -x -p alice 
"Hello bob from alice"
```

## Warning

My knownledge on cryptographic is quite limited and don't know if what I do is really that safe or useful so use it at your own risk.

