(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class Main {
   main() : Object {
      (new StackMachine).run()
   };
};

class CommandReader inherits IO {
   a2i: A2I <- new A2I;

   readCommand(): String {{
      
      in_string();
   }};

   getCommand(): Command {
      let commandStr: String <- readCommand() in 
         if commandStr.length() <= 0 then {
            abort();
            new Command;
         } else 
         if commandStr = "+" then new Plus else
         if commandStr = "s" then new Swap else
         if commandStr = "e" then new Eval else
         if commandStr = "d" then new Display else
         if commandStr = "x" then new Exit else
         parseVariable(commandStr)
         fi fi fi fi fi fi
   };

   parseVariable(str: String): Command {
      (new Variable).setValue(a2i.a2i(str))
   };
};

class StackMachine {
   stack: CommandStack <- new CommandStack;
   stopped: Bool <- false;
   reader: CommandReader <- new CommandReader;

   run(): Object {
      while not stopped loop 
         ((reader.getCommand())
            .init(self))
            .run()
      pool
   };

   stop(): Bool {
      stopped <- true
   };

   pushStack(cmd: Command): CommandStack {
      stack.push(cmd)
   };

   popStack(): Command {
      stack.pop()
   };

   stackIsEmpty(): Bool {
      stack.isEmpty()
   };

   stackHead(): CommandNode {
      stack.getHead()
   };
};

class Command {
   stackMachine: StackMachine;

   init(sm: StackMachine): SELF_TYPE {{
      stackMachine <- sm;
      self;
   }};
   
   run(): Object {
      self
   };
};

class StackedCommand inherits Command {

   run(): Object {
      stackMachine.pushStack(self)
   };

   to_string(): String {{
      abort();
      "Not implemented";
   }};
};

class Variable inherits StackedCommand {
   val: Int;

   setValue(v: Int): Variable {{
      val <- v;
      self;
   }};

   getValue(): Int {
      val
   };

   to_string(): String {
      (new A2I).i2a(val)
   };
};

class Plus inherits StackedCommand {
   to_string(): String {
      "+"
   };
};

class Swap inherits StackedCommand {
   to_string(): String {
      "s"
   };
};

class Eval inherits Command {
   run(): Object {
      if stackMachine.stackIsEmpty() then
         self
      else   
         case stackMachine.popStack() of
            var: Variable => stackMachine.pushStack(var);
            plus: Plus => 
               let i1: Int <- case stackMachine.popStack() of v: Variable => v.getValue(); esac, 
                   i2: Int <- case stackMachine.popStack() of v: Variable => v.getValue(); esac in
                  stackMachine.pushStack((new Variable).init(stackMachine).setValue(i1 + i2));
            
            swap: Swap => let v1: Command <- stackMachine.popStack(),
                              v2: Command <- stackMachine.popStack() in {
                  		stackMachine.pushStack(v1);
                  		stackMachine.pushStack(v2);
               };
         esac
      fi
   };
};

class Display inherits Command {
   io: IO <- new IO;

   run(): Object {
       let head: CommandNode <- stackMachine.stackHead() in {
           io.out_string("---\n");
           while not isvoid head loop {
               io.out_string("| ");
               case head.get() of c: StackedCommand => io.out_string(c.to_string()); esac;
               io.out_string("\n");
               head <- head.getNext();
           } pool; 
           io.out_string("---\n");
       }
   };	
};

class Exit inherits Command {
   run(): Object {
       stackMachine.stop()
   };
};

class CommandStack {
   head: CommandNode;

   isEmpty(): Bool {
      isvoid head
   };

   pop(): Command {
      if isvoid head then {
         abort();
         new Command;
      } else let oldHead: CommandNode <- head in {
         head <- oldHead.getNext();
         oldHead.get();
      } fi
   };

   push(value: Command): CommandStack {
      let newHead: CommandNode <- (new CommandNode).init(value) in {
         newHead.setNext(head);
         head <- newHead;
         self;
      }
   };

   getHead(): CommandNode {
      head
   };
};

class CommandNode {
   value: Command;
   next: CommandNode;

   init(val: Command): SELF_TYPE {{
      value <- val;
      self;
   }};

   get(): Command {
      value
   };

   set(val: Command): Command {
      value <- val
   };

   getNext(): CommandNode {
      next
   };

   setNext(node: CommandNode): CommandNode {
      next <- node
   };
};
