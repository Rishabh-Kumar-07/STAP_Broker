from Crypto import Random
from Crypto.Random import random
from Crypto.PublicKey import ElGamal
from Crypto.Util.number import GCD
import time

msg = b"ab"
msg = msg*1024

key = ElGamal.generate(1024, Random.new().read)

while 1:
    k = random.StrongRandom().randint(1, key.p - 1)
    if GCD(k, key.p - 1) == 1:
        break

modulus = str(getattr(key,'p'))
generator = str(getattr(key,'g'))
pub_key = str(getattr(key,'y'))
pri_key = str(getattr(key,'x'))

reply = "{{"+modulus+"},{"+generator+"},{"+pub_key+"},{"+pri_key+"}}"
print(reply)
