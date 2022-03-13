# Polinomios en SWI-Prolog

Proyecto para la materia de COM-23101 Inteligencia Artificial, Enero 2022, ITAM.

Desarrollada y probada con SWI-Prolog versión 8.4.1.

Usaremos listas de Prolog para definir polinomios de la siguiente
manera:  
Cada elemento de la lista representa el coeficiente de un término del polinomio y su posición en la lista representa el exponente del término correspondiente, ordenados de menor a mayor. Por ejemplo, la lista [1,2,3] representa al polinomio 3x^2 + 2x + 1; mientras que la lista [1,0,0,4] representa al polinomio 4x^4 + 1.

Las operaciones y métodos que se incluyen son
- Suma
- Resta
- Multiplicación
- Producto Escalar
- Composición
- Evaluación
- Diferenciación
- toString
