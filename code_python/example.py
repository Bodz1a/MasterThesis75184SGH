# Lokalizacja skryptu i ustawienie katalogu roboczego
import os

# dir_path = os.path.dirname(__file__)
# os.chdir(dir_path)
print(os.getcwd())

# Losowanie danych do przykładowej tabeli
import numpy as np
import random
import string

x_numbers = list(np.random.normal(0, 1, size=10))
x_letters = random.choices(string.ascii_letters, k=10)

# Tworzenie przykładowej tabeli
import pandas as pd

df = pd.DataFrame({'Class':x_letters, 'Values':x_numbers})
print(df)

# Zmiana formatu na LaTeX i eskport do pliku .tex
df_tex = df.style.hide(axis=0).to_latex(hrules=True)

tab_file = os.path.join(os.getcwd(), './paper/tabs/tab_02.tex')

with open(tab_file, 'w') as file:
    file.write(df_tex)
file.close()

# Tworzenie przykładowego histogramu
x_hist_sample = np.random.normal(0, 1, 1000)

from matplotlib import pyplot as plt

plt.clf()
plt.hist(x_hist_sample, bins=20, density=True)
plt.title('Example plot')

fig_file = os.path.join(os.getcwd(), './paper/figs/fig_02.png')
plt.savefig(fig_file)
