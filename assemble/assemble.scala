import scala.io.Source
import java.nio.file.{Paths, Files}
import scala.collection.mutable
import java.io.PrintWriter
import scala.util.control.Breaks


//getLines.next()はすべて読み込んでしまい自動でEmptyになる
//()()のようなラベルが連続している場合バグる

object Assemble{
	def main(args:Array[String]){
		if(args.size==0){
			println("No args")
		} else {
			println(args(0))
			val file=args(0)
			val xxx=Source.fromFile(file)
			for (line <- xxx.getLines){
				println(line)
			}
			val p0=new Parser(file)
			var i=0
			var l=false
			val t=new SymbolTable
			val wf = new PrintWriter(file.slice(0,file.size-3)+"hack")
			val ccc = new code
			val b = new Breaks
			var s0 =List[String]()
			//var check=0
			println(0)
			b.breakable{
			while(p0.hasMoreCommand()){
				//println(p0.hasMoreCommand())
				//最初に空白などが来た場合バグる
				p0.commandType() match{
					case (true,false,false) => {if(l){for(item<-s0){t.addEntry(item,i)};l=false;s0=List[String]()};i+=1;}
					case (false,true,false) => {if(l){for(item<-s0){t.addEntry(item,i)};l=false;s0=List[String]()};i+=1;}
					case (false,false,true) => {l=true;s0:+=p0.symbol()}
					case _ => println("?")
				}
				
				//check+=1
				//println(check)
				if(!(p0.advance())) b.break}
				//println(p0.hasMoreCommand())
			}
			println(t.table)
			val p1=new Parser(file)
			var j=16
			var out=""
			var s1=""
			println(t.table)
			b.breakable{
			while(p1.hasMoreCommand()){
				println(p1.commandType())
				p1.commandType() match{
					case (true,false,false) => {l=false;println("A");s1=p1.symbol();out= if(isNum(s1)) "0"+asNdigitBinary(s1.toInt,15) else {if(t.contains(s1)) "0"+asNdigitBinary(t.getAddress(s1),15) else {t.addEntry(s1,j);j+=1;"0"+asNdigitBinary(t.getAddress(s1),15)}}}
					case (false,true,false) => {l=false;println("C");out="111"+ccc.comp(p1.comp())+ccc.dest(p1.dest())+ccc.jump(p1.jump())}
					case (false,false,true) => {println("L");l=true}
					case (false,false,false) => println("comment")
				}
				println(s1)
				println(out)
				if(!out.isEmpty && !l) wf.write(out+"\n")
				//out to file
				if(!p1.advance()) b.break 
			}
			}
			p1.commandType() match{
					case (true,false,false) => {println("A");s1=p1.symbol();out= if(isNum(s1)) "0"+asNdigitBinary(s1.toInt,15) else {if(t.contains(s1)) "0"+asNdigitBinary(t.getAddress(s1),15) else {t.addEntry(s1,j);j+=1;"0"+asNdigitBinary(t.getAddress(s1),15)}}}
					case (false,true,false) => {println("Cc");out="111"+ccc.comp(p1.comp())+ccc.dest(p1.dest())+ccc.jump(p1.jump())}
					case (false,false,true) => println("L")
					case (false,false,false) => println("comment")
			}
			println(s1)
			println(out)
			if(!out.isEmpty)wf.write(out+"\n")
			wf.close()
		}
		
	}
	def isNum(d:String):Boolean={
		try{
			Integer.parseInt(d)
			return true
		} catch {case e:NumberFormatException => false}
	}
	def asNdigitBinary (source: Int, digits: Int): String = {
		val l: java.lang.Long = source.toBinaryString.toLong
		String.format ("%0" + digits + "d", l)
	}
	
	
	
class Parser(filename:String){
	println(filename,"what")
	val s0 = Source.fromFile(filename)
	val q=new mutable.Queue[String]
	for(line<-s0.getLines()){
		q.enqueue(line)
	}
	//val f = try s0.getLines.toList finally s0.close
	//val s1 = Source.fromFile(filename)
	var cur:String=q.dequeue
	
	
	def hasMoreCommand():Boolean ={
		!(q.isEmpty)
	}
	def advance():Boolean ={
		val t = q.dequeue
		cur = t.replace(" ","")//replace("//","").replace("/*","").replace("*/","")
		if(cur.contains("//")){if(cur(0)!='/' && cur.contains("//") && !cur.isEmpty) cur=cur.slice(0,cur.indexOf('/'))  else cur=""}
		print(cur,"現在の行")
		if(cur.isEmpty) {if(this.hasMoreCommand()) {println("空白、空行");advance()} else {println("コマンドなし");false}} else {println("コマンドあり");true}
	}
	def commandType():(Boolean,Boolean,Boolean) ={
		print(cur,"現在の行")
		cur(0) match {
			case '@' => (true,false,false) 
			case '(' => (false,false,true)
			case '/' => (false,false,false) //comment or else
			case _ => (false,true,false)
		}
	}
	def symbol():String ={
		cur(0) match {
			case '@' => cur.slice(1,cur.size)
			case '(' => cur.slice(1,cur.size-1)
			case _ => ""
		}
		
	}
	
	def dest():String ={
		if(cur.contains('=')){
			val n = cur.indexOf('=')
			cur.slice(0,n)
		} else ""
	}
	def comp():String ={
		val d= cur.contains('=')
		val c= cur.contains(';')
		(d,c) match {
			case (true,true) => {
				val dn = cur.indexOf('=')
				val cn = cur.indexOf(';')
				cur.slice(dn,cn)
			}
			case (true,false) => {
				val dn = cur.indexOf('=')
				cur.slice(dn+1,cur.size)
			}
			case (false,true) => {
				val cn = cur.indexOf(';')
				cur.slice(0,cn)
			}
			case _ =>
				cur
		
		}
	}
	def jump():String ={
		val d= cur.contains('=')
		val c= cur.contains(';')
		(d,c) match {
			case _ => {
				val dn = cur.indexOf('=')
				val cn = cur.indexOf(';')
				cur.slice(cn+1,cur.size)
			}
			case (_,false) => {
				""
			}
			
			
		}
	}
}


class code{
 	def dest(d:String):String ={
 		d match{
 			case "" => "000"
 			case c =>  {val d1=if(c.contains('A')) "1" else "0";val d2=if(c.contains('D')) "1" else "0";val d3=if(c.contains('M')) "1" else "0";d1+d2+d3}
 		}
 	}
 	def jump(j:String):String={
 		j match{
 			case "JGT" =>"001"
 			case "JEQ" => "010"
 			case "JGE" =>"011"
 			case "JLT" =>"100"
 			case "JNE" =>"101"
 			case "JLE" =>"110"
 			case "JMP" =>"111"
 			case _ => "000"
 		}
 	}
 	
 	def comp(c:String):String ={
 		val a:String=if(c.contains('M')) "1" else"0"
 		c match{
 			case "0" => a+"101010"
 			case "1" => a+"111111"
 			case "-1" => a+"111010"
 			case "D" => a+"001100"
 			case "A"|"M"=> a+"110000"
 			case "!D" => a+"001101"
 			case "!A"|"!M" => a+"110001"
 			case "-D" => a+"001111"
 			case "-A"|"-M" =>a+"110011"
 			case "D+1"|"1+M" => a+"011111"
 			case "A+1"|"M+1"|"1+A"|"1+M" => a+"110111"
 			case "D-1" =>a+"001110"
 			case "A-1"|"M-1" => a+"110010"
 			case "D+A"|"D+M"|"A+D"|"M+D" => a+"000010"
 			case "D-A"|"D-M" => a+"010011"
 			case "A-D"|"M-D" => a+"000111"
 			case "D&A"|"D&M"|"A&D"|"M&D" => a+"000000"
 			case "D|A"|"D|M"|"A|D"|"M|D" => a+"010101"
 		}
 	}
 }
 
class SymbolTable{
 	val table=mutable.Map("SP"->0,"LCL"->1,"ARG"->2,"THIS"->3,"THAT"->4,"SCREEN"->16384,"KBD"->24576)
 	for(i<- 0 to 15){
 		table.update("R"+i.toString,i)
 	}
 	def addEntry(symbol:String,address:Int):Unit={
 		table.update(symbol,address)
 	}
 	def contains(symbol:String):Boolean={
 		table.contains(symbol)
 	}
 	def getAddress(symbol:String):Int={
 		table(symbol)
 	}
 }
}
