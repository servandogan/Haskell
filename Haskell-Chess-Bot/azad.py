import random

buchstabe = [q, w, e, r, t, z, u, i, o, p, a, s, d, f, g, h, j, k, l, y, x, c, v, b, n, m]
grossbuchstabe = [Q, W, E, R, T, Z, U, I, O, P, A, S, D, F, G, H, J, K, L, Y, X, C, V, B, N, M]
zahlen = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]
zeichen = ("!§$%&/()=?")
ergebnis = []
a = random.choice(buchstabe)
b = random.choice(grossbuchstabe)
c = random.randint(zahlen)
d = random.choice(zeichen) 
f = random.choice(buchstabe)
g = random.choice(grossbuchstabe)
h = random.randint(zahlen)
i = random.choice(zeichen) 
j = (a, b, c, d, f, g, h, i)
k = random.choice(j)
l = random.choice(j)
m = random.randint(j)
n = random.choice(j) 
o = random.choice(j)
p = random.choice(j)
q = random.randint(j)
r = random.choice(j) 

e = input("Geben sie ihr neues Passwort ein oder lassen sie sich eins Generieren (dafür 1 eingeben): ")
if e == 1:
    ergebnis.append(k)
    ergebnis.append(l)
    ergebnis.append(m)
    ergebnis.append(n)
    ergebnis.append(o)
    ergebnis.append(p)
    ergebnis.append(q)
    ergebnis.append(r)
    print("Ihr neues Passwort ist" + ergebnis)
else:
    print("Ihr neues Passwort ist" + e)


