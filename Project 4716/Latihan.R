## Soal 1
Anuitas <- function(P,r,n){
  (P*r*(1+r)^n)/((1+r)^n-1)
}
P = 2000000
r = 0.03/12 # 3% setahun/ 12 bulan
n = 10*12 # 10 tahun x 12 bulan

# Jika dibayar di awal bulan
# Hitung nilai tunai atau cicilan tiap bulannya
A <- Anuitas(P=P,r=r,n=n)
print(A) # Ini adalah bunga cicilan setiap bulannya
Jumlah_Bunga_Awal_Bulan <- A*120
Total_Cicilan_Awal_Bulan <- P+Jumlah_Bunga_Awal_Bulan
print(Total_Cicilan_Awal_Bulan)
Cicilan_Per_Bulan_Awal_Bulan <- Total_Cicilan_Awal_Bulan/120
print(Cicilan_Per_Bulan_Awal_Bulan)

# Jika dibayar di akhir bulan
Jumlah_Bunga_Akhir_Bulan_Awal <- A*119
Cicilan_Terakhir <- P*r*(1+r)
Jumlah_Bunga_Akhir_Bulan <- Jumlah_Bunga_Akhir_Bulan_Awal+Cicilan_Terakhir
Total_Cicilan_Akhir_Bulan <- P+Jumlah_Bunga_Akhir_Bulan
print(Total_Cicilan_Akhir_Bulan)
Cicilan_Per_Bulan_Akhir_Bulan <- Total_Cicilan_Akhir_Bulan/120
print(Cicilan_Per_Bulan_Akhir_Bulan)

# Tabel perbandingan
Summary <- cbind(rbind(Jumlah_Bunga_Awal_Bulan,Jumlah_Bunga_Akhir_Bulan),
                 rbind(Total_Cicilan_Awal_Bulan,Total_Cicilan_Akhir_Bulan),
                 rbind(Cicilan_Per_Bulan_Awal_Bulan,Cicilan_Per_Bulan_Akhir_Bulan))
colnames(Summary) <- c("Jumlah Bunga","Total Hutang","Cicilan Per Bulan")
rownames(Summary) <- c("Awal Bulan","Akhir Bulan")
Summary
# Dapat dilihat bahwa, jika dibandingkan jumlah bunga, total hutang dan 
# cicilan per bulan yang lebih sedikit ada pada pembayaran di akhir bulan.
# Maka untuk cicilan tersebut sebaiknya diambil pembayaran akhir bulan.

## Soal 2
Cicilan_Bulanan <- function(P,r,n){
  (P*r/12)/(1-(1+r/12)^(-n*12))
}
P = 500000000 - 150000000 # Total pinjaman dikurang uang muka
n = 20 # 20 tahun
r = 0.04 # 4% setahun
Cicilan_tiap_bulan <- Cicilan_Bulanan(P=P,n=n,r=r)
print(paste("Cicilan yang harus dibayar setiap bulannya adalah",Cicilan_tiap_bulan,"rupiah"))

## Soal 3
PV <- function(PMT,r,n){
  PMT*((1-(1+r/12)^(-n*12))/(r/12))
}
PMT = 300000
r = 0.05
n = 20
Jumlah_Uang_Pensiun <- PV(PMT = PMT,r=r,n=n)
print(paste("Jumlah uang pensiunan yang akan diterima tiap bulannya adalah",
            Jumlah_Uang_Pensiun, "rupiah"))

## Soal 4
Cicilan_sudah_dibayar_per_bulan = 300000
Pinjaman_awal = 20000000
Bunga_tahunan = 0.05

Jumlah_cicilan_lunas_3_tahun = Cicilan_sudah_dibayar_per_bulan*12*3
Jumlah_cicilan_lunas_3_tahun

Sisa_pinjaman = Pinjaman_awal - Jumlah_cicilan_lunas_3_tahun
Sisa_pinjaman

Total_bunga = Sisa_pinjaman*0.05*3
Total_bunga

Sisa_hutang = Sisa_pinjaman+Total_bunga
print(paste("Sisa uang yang harus dibayar adalah",Sisa_hutang,"rupiah"))

# Soal 5
# Tahun 1 = P, Tahun 2 = P + P*0.05 = P*(1+0,05), Tahun 3 = (P*(1+0.05))(1+0.05) = P*(1+0.05)^2
# Maka dapat dibuat
P <- as.vector(matrix(0,ncol = 1,nrow = 15))
P[1] <- 1000 # Misal P = 1000
for (i in 1:15) {
  if (i<10){
    P[i+1]=P[i]+P[i]*0.05
  } else{
    P[i+1]=P[i]-P[i]*0.05
  }
}
P
