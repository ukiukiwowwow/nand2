import scala.io.Source
import java.nio.file.{Paths, Files}
import java.io.File
import scala.collection.mutable
import java.io.PrintWriter
import scala.util.control.Breaks

//eq,gt,lt,のラベルが複数同じものがある、毎回別のものにする必要がある

object vmtest{
	def main(args:Array[String]){
		if(args.size==0){
			println("No args")
		} else {
			println(args(0))
			var s:String=""
			var n=args(0).contains("/") match{
					case true => {s="/";args(0).lastIndexOf("/")+1}
					case false if(args(0).contains("\\")) => {s="\\";args(0).lastIndexOf("\\")+1}
					case _ => {s="0";0}
			}
			var dirname=args(0).slice(n,args(0).size)
			if(n==args(0).size){
				println(args(0).slice(0,args(0).size-1))
				n=args(0).contains("/") match{
					case true => {s="";args(0).slice(0,args(0).size-1).lastIndexOf("/")+1}
					case false if(args(0).contains("\\")) => {s="";args(0).slice(0,args(0).size-1).lastIndexOf("\\")+1}
					case _ => {s="0";0}
				}
				dirname=args(0).slice(n,args(0).size-1)
			}
			
			println(dirname)
			if(new File(args(0)).isFile()){
				val fn=args(0).slice(n,args(0).size-2)
				var p=new Parser(args(0))
				val wp = new PrintWriter(args(0).slice(0,n)+fn+"asm")
				val wf = new codeWriter(args(0),wp)
				process(p,args(0),wf)
				wf.close()
			} else {
				println(args(0)+s+dirname+".asm")
				println(dirname)
				val wp = new PrintWriter(args(0)+s+dirname+".asm")
				val wf = new codeWriter(args(0)+s+dirname+".asm",wp)
				for(item <- ls4(args(0),".vm")){
					
					//var file=Source.fromFile(item)
					var p=new Parser(item)
					process(p,item,wf)
				}
				wf.close()
			}
		}
	}
	
	def process(pfile:Parser,filename:String,w:codeWriter):Unit={
		//val w = new codeWriter(filename)
		w.setFileName(filename)
		while(pfile.hasMoreCommand()){
			println(1)
			pfile.commandType() match{
				case C_PUSH =>w.writePushPop("push",pfile.arg1(),pfile.arg2())
				case C_POP => w.writePushPop("pop",pfile.arg1(),pfile.arg2())
				case C_ARITHMETIC => {w.writeArithmetic(pfile.arg1());println("?")}
				case C_LABEL => w.writeLabel(pfile.arg1())
				case C_GOTO =>w.writeGoto(pfile.arg1())
				case C_IF =>w.writeIf(pfile.arg1())
				case _ => println("undefined")
			}
			if(!pfile.advance()) {return}
		}
		pfile.commandType() match{
			case C_PUSH =>w.writePushPop("push",pfile.arg1(),pfile.arg2())
			case C_POP => w.writePushPop("pop",pfile.arg1(),pfile.arg2())
			case C_ARITHMETIC => {w.writeArithmetic(pfile.arg1());println("?")}
			case C_LABEL => w.writeLabel(pfile.arg1())
			case C_GOTO =>w.writeGoto(pfile.arg1())
			case C_IF =>w.writeIf(pfile.arg1())
			case _ => println("undefined")
		}
		
	}
	
	def ls4(dir: String,fil:String) :Iterable[String] = {
		def ls(dir: String) : Seq[String] = {
			new File(dir).listFiles.flatMap {
				case f if f.isDirectory => ls(f.getPath)
				case x => List(x.toString)
			}
		}
		ls(dir).filter(_.endsWith(fil)).toIterable
	}
	
	
	sealed abstract class commandtype
	case object C_ARITHMETIC extends commandtype
	case object C_PUSH extends commandtype
	case object C_POP extends commandtype
	case object C_LABEL extends commandtype
	case object C_GOTO extends commandtype
	case object C_IF extends commandtype
	case object C_FUNCTION extends commandtype
	case object C_RETURN extends commandtype
	case object C_CALL extends commandtype
	case object BLANK extends commandtype
	
	class Parser(filename:String){
		val s0 = Source.fromFile(filename)
		val q=new mutable.Queue[String]
		for(line<-s0.getLines()){
			q.enqueue(line)
		}
		var cur:String=""
		this.advance()
		var spl:Array[String]=Array[String]()
		def hasMoreCommand():Boolean={!(q.isEmpty)}
		
		def advance():Boolean ={
			cur = q.dequeue
			if(cur.contains("//")){if(cur(0)!='/') cur=cur.slice(0,cur.indexOf('/')) else cur=""}
			print(cur,"現在の行")
			if(cur.isEmpty) {if(this.hasMoreCommand()) {println("空白、空行");advance()} else {println("コマンドなし");false}} else {println("コマンドあり");true}
		}
		
		def commandType():commandtype={
			println(cur)
			cur match{
				
				case temp if temp.startsWith("pop") => C_POP
				case temp if temp.startsWith("push") => C_PUSH
				case temp if temp.startsWith("label") => C_LABEL
				case temp if temp.startsWith("goto") => C_GOTO
				case temp if temp.startsWith("if-goto") => C_IF
				case temp if temp.startsWith("function") => C_FUNCTION
				case temp if temp.startsWith("call") => C_CALL
				case temp if temp.startsWith("return") => C_RETURN
				case "add"|"sub"|"neg"|"and"|"not"|"or"|"eq"|"lt"|"gt" => {println("?");C_ARITHMETIC}
				//case _ => BLANK
			}
		}
		
		def arg1():String ={
			spl= cur.split(" ")
			spl.size match {
				case 1 => spl(0)
				case _ => spl(1)
			}
		}
		def arg2():Int ={
			spl= cur.split(" ")
			spl.size match {
				case 3 => spl(2).toInt
				case _ => throw new RuntimeException("nocommand")
			}
		}
	}
	
	class codeWriter(filename:String,wf:PrintWriter){
		
		var start:Boolean=false
		var mem:String=_
		var i=0
		val n=filename.contains("/") match{
			case true => filename.lastIndexOf("/")+1
			case false if(filename.contains("\\")) => filename.lastIndexOf("\\")+1
			case _ => 0
		} 
		var name =""
		
		def setFileName(filename:String):Unit={
			name=filename.slice(n,filename.size-2)
		}
		
		def writeArithmetic(command:String):Unit={
			mem=command match{
				case "add" => "@SP\n"+"M=M-1\n"+"A=M\n"+"D=M\n"+"@SP\n"+"M=M-1\n"+"A=M\n"+"M=M+D\n"+"@SP\n"+"M=M+1\n"
				case "sub" => "@SP\n"+"M=M-1\n"+"A=M\n"+"D=M\n"+"@SP\n"+"M=M-1\n"+"A=M\n"+"M=M-D\n"+"@SP\n"+"M=M+1\n"
				case "neg" => "@SP\n"+"M=M-1\n"+"A=M\n"+"M=-M\n"+"@SP\n"+"M=M+1\n"
				case "and" => "@SP\n"+"M=M-1\n"+"A=M\n"+"D=M\n"+"@SP\n"+"M=M-1\n"+"A=M\n"+"M=M&D\n"+"@SP\n"+"M=M+1\n"
				case "or" => "@SP\n"+"M=M-1\n"+"A=M\n"+"D=M\n"+"@SP\n"+"M=M-1\n"+"A=M\n"+"M=M|D\n"+"@SP\n"+"M=M+1\n"
				case "not" => "@SP\n"+"M=M-1\n"+"A=M\n"+"M=!M\n"+"@SP\n"+"M=M+1\n"
				case "eq" => "@SP\n"+"M=M-1\n"+"A=M\n"+"D=M\n"+"@SP\n"+"M=M-1\n"+"A=M\n"+"D=M-D\n"+"@"+name+i.toString+".true\n"+"D;JEQ\n"+"@SP\n"+"A=M\n"+"M=0\n"+"@"+name+i.toString+".after\n"+"0;JMP\n"+"("+name+i.toString+".true)\n"+"@SP\n"+"A=M\n"+"M=-1\n"+"("+name+i.toString+".after)\n"+"@SP\n"+"M=M+1\n"
				case "gt"=>"@SP\n"+"M=M-1\n"+"A=M\n"+"D=M\n"+"@SP\n"+"M=M-1\n"+"A=M\n"+"D=M-D\n"+"@"+name+i.toString+".true\n"+"D;JGT\n"+"@SP\n"+"A=M\n"+"M=0\n"+"@"+name+i.toString+".after\n"+"0;JMP\n"+"("+name+i.toString+".true)\n"+"@SP\n"+"A=M\n"+"M=-1\n"+"("+name+i.toString+".after)\n"+"@SP\n"+"M=M+1\n"
				case "lt"=> "@SP\n"+"M=M-1\n"+"A=M\n"+"D=M\n"+"@SP\n"+"M=M-1\n"+"A=M\n"+"D=M-D\n"+"@"+name+i.toString+".true\n"+"D;JLT\n"+"@SP\n"+"A=M\n"+"M=0\n"+"@"+name+i.toString+".after\n"+"0;JMP\n"+"("+name+i.toString+".true)\n"+"@SP\n"+"A=M\n"+"M=-1\n"+"("+name+i.toString+".after)\n"+"@SP\n"+"M=M+1\n"
			}
			println(mem)
			wf.write(mem)
			i=i+1
			
		}
		
		
		def writePushPop(command:String,arg:String,index:Int):Unit={
			//
			mem=arg match{
				case "argument" => "@"+index.toString+"\n"+"D=A\n"+"@ARG\n"+"A=M+D\n"
				case "local" => "@"+index.toString+"\n"+"D=A\n"+"@LCL\n"+"A=M+D\n"
				case "constant" => "@"+index.toString+"\n"+"D=A\n"+"@ARG\n"+"A=M+D\n"
				case "this" => "@"+index.toString+"\n"+"D=A\n"+"@THIS\n"+"A=M+D\n"
				case "that" => "@"+index.toString+"\n"+"D=A\n"+"@THAT\n"+"A=M+D\n"
				case "pointer" => "@"+index.toString+"\n"+"D=A\n"+"@THIS\n"+"A=A+D\n"
				case "temp" => "@"+index.toString+"\n"+"D=A\n"+"@R5\n"+"A=A+D\n"
				case "static" => "@"+name+"."+index.toString+"\n"
			}
			
			println(mem)
			if(arg=="constant"){
				wf.write("@"+index.toString+"\n"+"D=A\n"+"@SP\n"+"A=M\n"+"M=D\n"+"@SP\n"+"M=M+1\n")
				return
			}
			
			
			wf.write(
			command match{
				//case "pop" => {"@SP\n"+"A=M-1\n"+"D=M\n"+mem+"D=A\n"+"@SP\n"+"A=M-1\n"+"M=D\n"}
				case "pop" =>{mem+"D=A\n"+"@SP\n"+"A=M\n"+"M=D\n"+"A=A-1\n"+"D=M\n"+"A=A+1\n"+"A=M\n"+"M=D\n"+"@SP\n"+"A=M\n"+"M=0\n"+"@SP\n"+"M=M-1\n"}
				case "push" => {mem+"D=M\n"+"@SP\n"+"A=M\n"+"M=D\n"+"@SP\n"+"M=M+1\n"}
			})
			
		}
		
		def writeLabel(label:String):Unit={
			wf.write("("+label+")\n")
		}
		
		def writeGoto(label:String):Unit={
			wf.write("@"+label+"\n"+"0;JMP\n")
		}
		
		def writeIf(label:String):Unit={
			wf.write("@SP\n"+"M=M-1\n"+"A=M\n"+"D=M\n"+"@"+label+"\n"+"D;JNE\n")
		}
		
		def close():Unit={
			wf.write("(END)\n"+"@END\n"+"0;JMP\n")
			println(name,n)
			wf.close()
		}
	}
	
}
