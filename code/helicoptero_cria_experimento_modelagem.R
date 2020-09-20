#cria um experimento randomico para modelar o helicoptero
helicopterodesign <- expand.grid(
  Altura = gl(2,1, labels = c("1.30","2.10")),
  Clipe = gl(2,1, labels = c("-","+")),
  AdesivoTopo = gl(2,1, labels = c("-","+")),
  AdesivoLateral = gl(2,1, labels = c("-","+")))
helicopterodesign
helicopterodesign$ordem <- sample(1:16,16)
helicopterodesign <- helicopterodesign[
  order(helicopterodesign$ordem),]
helicopterodesign
