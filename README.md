# My diploma work at Kupittaa upper secondary school (2011)

## Original readme:

```
Usage: rsa-tool -m [MESSAGE]  (Generate random keys)

* * *

List of optional arguments:
 --publicKey [INTEGER] [INTEGER], -a [INTEGER] [INTEGER]
                                    Set the public key
 --privateKey [INTEGER] [INTEGER], -b [INTEGER] [INTEGER]
                                    Set the private key
 --message [MESSAGE], -m [MESSAGE]
                                    Message to be crypted/uncrypted
 --range [INTEGER] [INTEGER], -r [INTEGER] [INTEGER]
                                    Generate primes in range [a,b]
 --phase [INTEGER], -l [INTEGER]    
                                    Set the phase used in encryption/decryption
                                    (default = 3)
 -p [PRIME] -q [PRIME]
                                    Use PRIMEs to generate keys. The bigger the
                                    better. -p and -q must be given in this
                                    order
 --breakPrivateKey, -B
                                    Break the encryption. --publicKey or -a is
                                    required
 --verbose, -v
                                    Verbose mode

* * *

Examples:

Generate keys and crypt message using two big prime numbers:
 rsa-tool -p 5915587277 -q 1500450271 --verbose -m secret
 $> Flags given: [Verbose]
    Just (PublicKey 7 8876044532898802067)
    Just (PrivateKey 12680063607832520743 8876044532898802067)
    Phase length: 3
    Primes generated in range (1000,500000)
    Given message: secret
    28831976414719312525275673650673082748

Decrypt previous message:
 rsa-tool --privateKey 12680063607832520743 8876044532898802067
		  -m 28831976414719312525275673650673082748

 $> secret
 ```
