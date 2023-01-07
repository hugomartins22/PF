

data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]


horavalida :: Hora -> Bool
horavalida (H h m) | (h>=0 && h<=23) && (m>=0 && m<=59) = True
                   | otherwise = False 



auxEtapa :: Etapa -> Bool
auxEtapa ((H hi mi),(H hf mf)) | (hf==hi) && (mf>mi) = True
                               | (hf>hi) = True
                               | otherwise = False



--a)
goodEtapa :: Etapa -> Bool
goodEtapa (hi,hf) | (a == True) && (b == True) && (c == True) = True
                  | otherwise = False 
                  where a = horavalida hi
                        b = horavalida hf
                        c = auxEtapa (hi,hf)


--b)
testTravel :: Viagem -> Bool
testTravel [] = True
testTravel [e] = goodEtapa e
testTravel (e1:e2:es) | (((goodEtapa e1) == True) && ((auxTravel e1 e2) == True)) = testTravel (e2:es)
                      | otherwise = False
                   


auxTravel :: Etapa ->Etapa -> Bool
auxTravel ((H hi mi),(H hf mf)) ((H h2i m2i),(H h2f m2f)) | (hf==h2i) && (mf<m2i) = True
                                                          | (hf<h2i) = True
                                                          | otherwise = False




--c)
horasTravel :: Viagem -> (Hora,Hora)
horasTravel (l:ls) = ((auxHorasi l),(auxHorasf (last (l:ls))))


auxHorasi :: Etapa -> Hora
auxHorasi (x,y) = x

auxHorasf :: Etapa -> Hora
auxHorasf (x,y) = y




--d)
efectiveTime :: Viagem -> Hora
efectiveTime l = (H a b)
               where a = (somaHoras l)
                     b = (somaMin l)



somaHoras :: Viagem -> Int
somaHoras [] = 0
somaHoras (l1:ls) = (difHoras l1) + somaHoras ls 


somaMin :: Viagem -> Int
somaMin [] = 0
somaMin (l1:ls) = (difMin l1) + somaMin ls 


difHoras :: Etapa -> Int
difHoras ((H hi mi),(H hf mf)) = hf-hi


difMin :: Etapa -> Int
difMin ((H hi mi),(H hf mf)) = mf-mi




--e)
queueTime :: Viagem -> Hora
queueTime l = convertor (recMinQueue l)


convertor :: Int -> Hora
convertor x = (H (div x 60) (mod x 60))



minQueue :: Etapa ->Etapa -> Int
minQueue ((H hi mi),(H hf mf)) ((H h2i m2i),(H h2f m2f)) | (h2i>hf) && (mf==m2i) = (h2i-hf)*60
                                                         | (h2i==hf) && (mf<m2i) = (m2i-mf)
                                                         | otherwise = (h2i-hf)*60 + (m2i-mf)



recMinQueue :: Viagem -> Int
recMinQueue [] = 0
recMinQueue [x] = 0
recMinQueue (l1:l2:ls) = (minQueue l1 l2) + recMinQueue (l2:ls)
                      



--f)
tempoTotal :: Viagem -> Hora
tempoTotal l = convertor a
             where a = convertor2 (auxTempoTotal (queueTime l) (efectiveTime l))
 

auxTempoTotal :: Hora -> Hora -> Hora
auxTempoTotal (H h1 m1) (H h2 m2) = (H (h1+h2) (m1+m2))


convertor2 :: Hora -> Int
convertor2 (H x y) = x*60 + y






--2)

type Poligonal = [Ponto]

data Ponto = Cartesiano Double Double | Polar Double Double
                     deriving (Show,Eq)




--a)
compLinha :: Poligonal -> Double
compLinha [] = 0
compLinha [x] = 0
compLinha (l1:l2:ls) = (distancia l1 l2) + compLinha (l2:ls)  




distancia :: Ponto -> Ponto -> Double
distancia (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt (((x2-x1)^2)+((y2-y1)^2))




--b)
--checaCoords :: Poligonal -> Bool
--checaCoords ((Cartesiano x y):ls) | ((length ((Cartesiano x y):ls)) <= 2) = False
--                                  | ((elem (Cartesiano (n*x) (n*y)) ls) == True) = False
--                                  | ((elem (Cartesiano x (n*y)) ls) == True) = False
--                                  | ((elem (Cartesiano (n*x) y) ls) == True) = False
--                                  | otherwise = True
--                            where n = [1..9]





data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)



data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show


type Nome = String
type Agenda = [(Nome, [Contacto])]

--3)
--a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n e a = [(n,[Email e])] ++ a





--b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((l,[a,b,c,Email e]):ls) | (procurarName n ((l,[a,b,c,Email e]):ls) == True) = Just [e]
                                     | otherwise = (verEmails n ls) 



procurarName :: Nome -> Agenda -> Bool
procurarName n [] = False
procurarName n ((l,x):ls) | (n==l) = True
                          | otherwise = False





--c)

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (l:ls) = case l of (Casa x) -> (x : consTelefs ls)
                              (Trab x) -> (x : consTelefs ls)
                              (Tlm x) -> (x : consTelefs ls)
                              otherwise -> (consTelefs ls)




--d)

--casa :: Nome -> Agenda -> Maybe Integer
--casa n [] = Nothing
--casa n ((l,x):ls) | (l == n) = Just (procurarFixo x)
--                  | otherwise = casa n ls



procurarFixo :: [Contacto] -> [Integer]
procurarFixo [] = []
procurarFixo (c:cs) = case c of Casa x -> x : procurarFixo cs
                                otherwise -> procurarFixo cs





--4)

type Dia = Int
type Mes = Int
type Ano = Int

data Data = D Dia Mes Ano
          deriving Show

type TabDN = [(Nome,Data)]



--a)

procura :: Nome -> TabDN -> Maybe Data
procura name [] = Nothing
procura name ((n,d):ls) | (name==n) = Just d 
                        | otherwise = procura name ls

--b)

procura2 :: Nome -> TabDN -> Data
procura2 name ((n,d):ls) | (name==n) = d 
                         | otherwise = procura2 name ls



idade :: Data -> Nome -> TabDN -> Maybe Int
idade d n [] = Nothing
idade (D dia mes ano) name l | ((auxIdade name l)==True) = Just (anos (D dia mes ano) (procura2 name l))
                             | otherwise = Nothing


-- ja fez anos no presente ano?
jaFezAnos :: Data -> Data -> Bool
jaFezAnos (D dia mes ano) (D dia2 mes2 ano2) | mes2 > mes = False
                                             | (mes2 == mes) && (dia2 > dia) = False
                                             | otherwise = True


-- verificar se existe o nome referido dentro da lista das datas
auxIdade :: Nome -> TabDN -> Bool
auxIdade name [] = False
auxIdade name ((n,d):ls) | (name==n) = True
                         | otherwise = auxIdade name ls




anos :: Data -> Data -> Int
anos (D dia mes ano) (D dia2 mes2 ano2) | (jaFezAnos (D dia mes ano) (D dia2 mes2 ano2)) == False = ((ano-ano2)-1)
                                        | otherwise = (ano-ano2)




--c)
anterior :: Data -> Data ->Bool
anterior (D dia2 mes2 ano2) (D dia mes ano) | ano>ano2 = True
                                            | (ano==ano2) && (mes>mes2) = True
                                            | (ano==ano2) && (mes==mes2) && (dia>dia2) = True
                                            | otherwise = False




--d)
ordena :: TabDN -> TabDN
ordena [] = []
ordena [x] = [x]
ordena ((n,d):(n2,d2):ls) | (a == True) = (n,d) : ordena ((n2,d2):ls)
                          | otherwise = ordena (((n2,d2):ls) ++ [(n,d)])
                       where a = anterior d d2 




--e)
porIdade :: Data -> TabDN ->[(Nome,Int)]
porIdade datas ((n,d):ls) = ordena2 c
                         where c = auxiliar datas ((n,d):ls)



ordena2 :: [(Nome,Int)] -> [(Nome,Int)]
ordena2 [] = []
ordena2 [x] = [x]
ordena2 ((n,i):(n2,i2):ls) | (i>i2) = (n,i) : ordena2 ((n2,i2):ls)
                           | otherwise = ordena2 ((n2,i2):ls) ++ [(n,i)]


auxiliar :: Data -> TabDN -> [(Nome,Int)]
auxiliar d [] = []
auxiliar datas ((n,d):ls) = (n,(idade2 datas n ((n,d):ls))) : auxiliar datas ls


idade2 :: Data -> Nome -> TabDN -> Int
idade2 d n [] = 0
idade2 (D dia mes ano) name l | ((auxIdade name l)==True) = anos (D dia mes ano) (procura2 name l)
                              | otherwise = 0







--5)

data Movimento = Credito Float | Debito Float
               deriving Show
data Data = D Int Int Int
          deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show




--a)
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext m ((d,s,(move x)):ls)) y | y>=  



auxvalor :: Extracto -> Float
auxvalor (Ext m ((d,s,(move x)):ls)) = x



































