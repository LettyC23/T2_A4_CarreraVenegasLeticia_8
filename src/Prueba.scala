
class Paciente(nombre:String, primerAp:String,segundoAP:String, edad:Byte, val fecha:Array[String], val horaRegistro:Array[String],val nivelBienestar:Array[Byte], val temperatura:Array[Double],val humedad:Array[Double]) {
  
   def imprimir(): Unit = {
    println("Nombre: " + nombre)
    println("Primer Apellido: " + primerAp)
    println("Segundo Apellido: " + segundoAP)
    println("Edad: " + edad)
    println("Fechas: ")
    fecha.foreach(println)
    println("Hora de registro: ")
    horaRegistro.foreach(println)
    println("Nivel de bienestar: ")
    nivelBienestar.foreach(println)
    println("Temperaturas: ")
    temperatura.foreach(println)
    println("Humedades: ")
    humedad.foreach(println)
  }
  
   //---------------------METODO PROMEDIO NIVEL DE BIENESTAR--------------------------------------
  def promedioNivelBienestar(): Int ={
    var suma = 0
    for (i <- 0 until nivelBienestar.length) {
      suma += nivelBienestar(i)
    }
    (suma / nivelBienestar.length)
  }
  
  //---------------------------------MÉTODO TEMPERATURA MAYOR--------------------------------
  def temperaturaMayor(): Unit={
    
    var mayor = 0
    for (i <- 0 until temperatura.length) {
      if (temperatura(i) > mayor) {
        mayor = i
      }
    }
    println("---------------TEMPERATURA MAYOR-----------------------")
    println("Temperatura mayor: " + temperatura(mayor)+" °C")
    println("Fecha: " + fecha(mayor) + "\nHora: " + horaRegistro(mayor))
    println("Nivel de bienestar: " + nivelBienestar(mayor))
    print("Humedad: " + humedad(mayor) + " %")
    println("")
  }
  
  
  //----------------------MÉTODO TEMPERATUURA MENOR-----------------------------
  def temperaturaMenor(): Unit={
    var menor = 0
    for (i <- 0 until temperatura.length) {
      if (temperatura(i) < menor) {
        menor = i
      }
    }
   println("--------------------TEMPERATURA MENOR---------------------------------")
    println("Temperatura menor: " + temperatura(menor) + " °C")
    println("Fecha: " + fecha(menor) + "\nHora: " + horaRegistro(menor))
    println("Nivel de bienestar: " + nivelBienestar(menor))
    print("Humedad: " + humedad(menor)+" %")
    println("")
  }
  
}


object Prueba {
  //-----------------------LLENAR VECTOR FECHAS------------------------------
   def llenarVectorFechas(n: Int) = {
    var fechas = new Array[String](n)
    for (i <- 0 to fechas.length-1) {
      var dia = 0; 
      val año = (1950 + math.random*( 2021 - 1950)).toInt;
      val mes = (1 + math.random*( 12 - 1)).toInt;
      
      mes match{
      case 1 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 2 => 
        val x =0;
        val h = 1950;
        for( h <- 1950 to 2021 by + 4){
        if(h.equals(año)){
          x==1;
          h==2021;
        }
      }
        if(x==0)(dia = (1 + math.random*( 28 - 1)).toInt)
        else(dia = (1 + math.random*( 29 - 1)).toInt)
      case 3 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 4 => (dia = (1 + math.random*( 30 - 1)).toInt)
      case 5 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 6 => (dia = (1 + math.random*( 30 - 1)).toInt)
      case 7 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 8 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 9 => (dia = (1 + math.random*( 30 - 1)).toInt)
      case 10 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 11 => (dia = (1 + math.random*( 30 - 1)).toInt)
      case 12 => (dia = (1 + math.random*( 31 - 1)).toInt)
      
      }
      fechas(i) = dia + "-" + mes + "-" + año
    }
    fechas
  }
  
   
   //-----------------------------LLENAR VECTOR HORA DE REGISTRO-----------------------
  def llenarVectorHoraRegistro(n: Int) = {
    var horaRegistro = new Array[String](n)
    for (i <- 0 until horaRegistro.length) {
      val horas = (0 + math.random*( 23 - 0)).toInt
      val minutos = (0 + math.random*( 59 - 0)).toInt
      horaRegistro(i) = horas + ":" + minutos
    }
    horaRegistro
  }
  
  
  //---------------------LLENAR VECTOR NIVEL DE BIENESTAR (1-5)---------------------------
  def llenarVectorNivelBienestar(n: Int) = {
    var nivel = new Array[Byte](n)
    for (i <- 0 until nivel.length) {
      nivel(i) = (1 + math.random*( 5 - 1)).toByte
    }
    nivel
  }
  
  
  //----------------------LLENAR VECTOR TEMPERATURAS---------------------------------
  def llenarVectortemperaturas(n: Int) = {
    var temp = new Array[Double](n)
    for (i <- 0 until temp.length) {
      temp(i) = (-15 + math.random*( 50 +15))
    }
    temp
  }
  
    
    //-----------------LLENAR VECTOR HUMEDADES-------------------------------------------
    def llenarVectorHumedad(n: Int) = {
    var humedad = new Array[Double](n)
    for (i <- 0 until humedad.length) {
      humedad(i) = (0 + math.random*( 100 - 0))
    }
    humedad
  }
    
    
   
  def main(args: Array[String]): Unit = {
    println("Ingrese el Nombre: ")
    val nombre = readLine()
    println("Ingrese el Primer Apellido: ")
    val primerAp = readLine()
    println("Ingrese el Segundo Apellido: ")
    val segundoAp = readLine()
    println("Ingrese la Edad: ")
    val edad = readByte()
    println("Ingrese número de datos que desea ingresar a los vectores: ")
    val num = readInt()
    var paciente = new Paciente(nombre, primerAp, segundoAp, edad, llenarVectorFechas(num), 
        llenarVectorHoraRegistro(num), llenarVectorNivelBienestar(num), llenarVectortemperaturas(num), llenarVectorHumedad(num));
    
    
    paciente.imprimir()
    println("Promedio de Nivel de bienestar: "+paciente.promedioNivelBienestar())
    println("")
    paciente.temperaturaMayor()
    println("")
    paciente.temperaturaMenor()
    println("")
    
  } 
}