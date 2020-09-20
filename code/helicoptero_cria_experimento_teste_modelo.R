#cria um experimento randomico para teste do modelo
helicopteromodeltestdesign <- expand.grid(Altura = gl(4,1, labels = c("1.00","1.50","1.70","2.20")),
                          Clipe = gl(2,1, labels = c("-","+")))
helicopteromodeltestdesign
helicopteromodeltestdesign$ordem <- sample(1:8,8)
helicopteromodeltestdesign <- helicopteromodeltestdesign[order(helicopteromodeltestdesign$ordem),]
helicopteromodeltestdesign
