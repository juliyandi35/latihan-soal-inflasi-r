### Soal Latihan ###
## Soal 1
Nt <- function(R,r,n){ #Rumus Nilai Tunai
  R*(1-(1/((1+r)^n)))/r
}
Nap <- function(R,r,n){ # Rumus Nilai Akhir Pembayaran
  R*(1/((1+r)^n))
}

# Jika r=5%
Nilai_tunai.5 = Nt(5000000,0.05,10)
Nilai_akhir_pembayaran.5 = Nap(5000000,0.05,10)
print(paste("Nilai tunai pembayaran adalah",Nilai_tunai.5,"rupiah dan nilai akhir pembayaran adalah",Nilai_akhir_pembayaran.5,"rupiah"))

# Jika r=8%
Nilai_tunai.8 = Nt(5000000,0.08,10)
Nilai_akhir_pembayaran.8 = Nap(5000000,0.08,10)
print(paste("Nilai tunai pembayaran adalah",Nilai_tunai.8,"rupiah dan nilai akhir pembayaran adalah",Nilai_akhir_pembayaran.8,"rupiah"))

## Soal 2
Jumlah_Awal <- function(A,r,t){
  A/(1+r*t)
}

# Rentang pembayaran di tahun ke-6 sampai 12. Sehingga terdapat 7 tahun
# Setiap tahun membayar 8 juta. Maka untuk 12 tahun totalnya adalah
# 7x8 = 56 juta. Maka jumlah yang harus ditaruh adalah
Jumlah_tabungan_awal <- Jumlah_Awal(56000000,0.12,12)
print(paste("Jumlah yang harus ditaruh ayah adalah",Jumlah_tabungan_awal,"rupiah"))

## Soal 3
B = 0.03 # Bunga tahunan 3%
Cpb = 5000000 # Cicilan per bulan
Cpt = Cpb*12 # Cicilan per tahun
Cpt10 = Cpt*10 # Total cicilan 10 tahun
Bpt10 = Cpt*B*10 # Bunga per tahun
Total = Cpt10+Bpt10 # Total harga rumah
print(paste("Total harga rumah adalah",Total,"rupiah"))

## Soal 4
PV <- function(R,i,n,m){ #Rumus nilai tunai
  (R/((1+i)^n))*(1-(1/((1+i)^m)))
}
FV <- function(PV,i,n){ # Rumus nilai akhir
  PV*(1+i)^n
}

R = 100
i = 0.08
n = 10
m = 5

Nt <- PV(R,i,n,m) # Nilai tunai
print(paste("Jumlah nilai tunainya adalah",Nt,"rupiah"))
Na <- FV(Nt,i,n) # Nilai akhir
print(paste("Jumlah nilai akhirnya adalah",Na,"rupiah"))

## Soal 5
Tabungan <- as.vector(matrix(0,ncol = 1,nrow = 15))
Tabungan[1] <- 1000
for (i in 1:14) { # 1:15 karena kita ingin menghitung nilai tabungan di tahun ke-15
  Tabungan[i+1] <- Tabungan[i]+(0.05*Tabungan[i])
  print(Tabungan[i+1])
}
print(paste("Jumlah uang di akhir tahun ke-15 adalah",Tabungan[15],"rupiah"))

## Soal 6
Anuitas <- function(P,r,n){
  (P*r*(1+r)^n)/((1+r)^n-1)
}
P = 300000000 - 100000000 # Jumlah pinjaman dikurang uang muka
r = 0.04/12 # 4% dibagi 12 bulan
n = 15*12 # 15 tahun dikali 12 bulan

Bb <- Anuitas(P,r,n)
Cb <- (P/n)+Bb
print(paste("Cicilan bulanan yang harus dibayar adalah",Cb,"rupiah"))

## Soal 7
PV <- function(FV,r,n){
  FV/((1+r)^n)
}
FV = 10000000
r = 0.025
n = 10

St <- PV(FV,r,n)
print(paste("Jumlah setoran per tahun adalah",St,"rupiah"))
