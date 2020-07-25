import time
import telnetlib
from Crypto import Random
from Crypto.Random import random
from Crypto.PublicKey import ElGamal
from Crypto.Util.number import GCD
from Crypto.Cipher import AES
import time
import string

def get_random_string(length):
    letters = string.ascii_lowercase
    result_str = ''.join(random.choice(letters) for i in range(length))
    return(result_str)

HOST = "localhost"
tn = telnetlib.Telnet(HOST, 1055)
No_server = int(input("Enter no. of server"))
Server_key = []

def generate_reply(identifier, ciphertext):
  s1 = "shuffle:first_shuffle_setup("+str(identifier)+","
  s2 = ")."
  reply = s1 + ciphertext + s2
  reply = bytes(reply, "utf-8")
  return(reply)

def resolve(cip):
  list = cip.split("joining_string") 
  for i in list:
   bytes.fromhex(i)

def format_ciphertext(ciphertext):
  flag = "joining_string"
  j = ""
  for i in ciphertext:
    j = j + flag + i.hex()
  return(j)

for i in range(0, No_server):
  m = int(input("Enter modulus"))
  g = int(input("Enter generator"))
  p = int(input("Enter public key")) 
  q = int(input("Enter private key"))
  key_obj = (m,g,p,q)
  key = ElGamal.construct(key_obj)
  Server_key.append(key)


IV = []
AES_Key = []

for m in range(0,10):
  aes_key = bytes(get_random_string(32),"utf-8")
  iv = Random.new().read(AES.block_size)
  cipher = AES.new(aes_key, AES.MODE_CFB, iv)
  IV.append(iv)
  AES_Key.append(cipher)
  for i in range(0, No_server):
    skey = Server_key[i]
    while 1:
      k = random.StrongRandom().randint(1,skey.p-1)
      if GCD(k,skey.p-1)==1: 
        break
    ciphertext = skey.encrypt(aes_key,k)
    print("Before encryption: ",aes_key)
    print("After encryption: ",ciphertext)
    plaintext = skey.decrypt(ciphertext)
    print("After decryption: ",plaintext)
    aes_key = format_ciphertext(ciphertext)
  reply = generate_reply(m, aes_key)
  print(reply)

  #send = bytes("shuffle:first_shuffle_setup("+str(i)+","+str(i+100)+").", "utf-8")
  tn.write(reply)
  time.sleep(0.2)

#time.sleep(20)
