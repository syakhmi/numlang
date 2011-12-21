#This is a makefile

SRCDIR= ./com/numlang
BINDIR= ../bin

JAVAC= javac
JOPTS= -d $(BINDIR)
JMORE= -Xlint


SOURCE= $(SRCDIR)/BigRational.java\
	$(SRCDIR)/NumValue.java\
	$(SRCDIR)/FuncValue.java\
	$(SRCDIR)/NumLang.java\
	$(SRCDIR)/ListValue.java\
	$(SRCDIR)/MatrixValue.java\
	$(SRCDIR)/StringValue.java\
	$(SRCDIR)/testmain.java

default: $(SOURCE)
	$(JAVAC) $(JOPTS) $(SOURCE)

safe: $(SOURCE)
	$(JAVAC) $(JOPTS) $(JMORE) $(SOURCE)

clean:
	rm $(BINDIR)/$(SRCDIR)/*.class
