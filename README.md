# Árboles de decisión en machine learning: lecciones sobre el trabajo en equipo.
El 12 de mayo di una plática en el Taller Estudiantil de Cómputo de la Facultad de Ciencias, donde hablé de mi tesis en Random Forest y Boosting.

El material incluye las slides de la presentación, divididas en tres partes debido a que el archivo original supera el peso límite de GitHub. En ellas se da una introducción a machine learning con un enfoque estadístico, y se presentan los algoritmos CART, bagging, Random Forest y gradient boosting, acompañado de "toy examples".

El archivo cali.csv es la base de datos California Housing usada para los ejemplos prácticos en R. 
El código trees.R explica el mecanismo de árboles de regresión, y se usa la biblioteca rpart para crear árboles como el siguiente:
<img width="609" alt="cap2_arbol_800" src="https://user-images.githubusercontent.com/73151688/118307606-8abfc700-b4b0-11eb-81e8-3a5d6336b022.png">

Y la observación de la varianza y sesgo:
<img width="911" alt="Screen Shot 2021-05-14 at 12 40 47" src="https://user-images.githubusercontent.com/73151688/118308504-a4154300-b4b1-11eb-9d97-aa9a9ebd32c9.png">

El código RandomForest.R implementa algoritmos de bagging y Random Forest con la biblioteca randomForest. En Boosting1.R y Boosting2.R se implementa boosting por medio de la interfaz h2o, y se hacen comparaciones entre dos funciones de pérdida distintas. Algunas gráficas ilustrativas son las siguientes:

<img width="464" alt="Cali_rf_mtry_test" src="https://user-images.githubusercontent.com/73151688/118307910-f1dd7b80-b4b0-11eb-8e29-60a0a2ee9c90.png">
<img width="496" alt="Cali_gbm_tst" src="https://user-images.githubusercontent.com/73151688/118307929-f73ac600-b4b0-11eb-8503-bcd6d96deb8a.png">
<img width="680" alt="Cali_predictions" src="https://user-images.githubusercontent.com/73151688/118308004-0c175980-b4b1-11eb-8d64-de6d489c819d.png">.

trees.R fue creado especialmente para la plática, mientras que el resto es parte de mi tesis. Por alguna razón no puedo subir los .RData para que no tengan que correr el código y usen lo que yo ya hice, si alguien los quiere, puede enviarme un correo.
Cualquier duda la responderé con gusto, tan bien como pueda, a través de cortesruiz@ciencias.unam.mx
