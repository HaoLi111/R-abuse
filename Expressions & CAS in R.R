#Expressions-function-body-list yacas
#CAS
#simbolic calculations
#-----------------------------------------------
#Visualize &exhausive calculations
y<-function(x) x^2
y(1:20)
xbase=seq(from=-5,to=5,by=.01)
xbase
plot(xbase,y(xbase),type='l')
plot(xbase,y(xbase),type = 'l',asp =1 )
ymin<-min(y(xbase))
xbase[which(y(xbase)==ymin)]
#
#D(y,'x')
#D(y,'x',function.argument = T)
#---------------------------------------------
#Functions &Expressions
body(y)
y_s<-as.expression(body(y))
y_s
D(y_s,'x')
dydx_1<-D(y_s,'x')
dydx_1
class(dydx_1)
dydx_1(1)
dydx_2<-deriv(y_s,'x')
dydx_2
summary(dydx_2)
#Evaluation of R expressions and assignment of variables
x<-1
eval({dydx_2})
x<-2
eval({dydx_2})
remove(x)#kill(x) in maxima CAS


#Evaluation in broad term

plotiris<-expression(plot(iris))
eval({plotiris})
plot(iris)#which is equivalent
#parse expression from text in broad term
statement='f<-function(y) y+12 '
eval({parse(text = statement)})
f
#so to run code in text
runText<-function(texttrun) eval({parse(text = texttrun)})
runText('1+1')
runText('contour(volcano)')

h<-parse(text = 'x-y^2')
h
#get() parse() and deparse()
x<-1
x
print(x)
get('x')
y_s
eval({x<-0;y_s})
eval({x=0;dydx_2})
eval({x=(-1);y_s})
eval({x=(-1);dydx_2})
#
integrate(y,-1,1)
#---------------------------------------------------------
library(Deriv)
dydx_1#D() mainly used for printing out the result
dydx_2#deriv() takes expression, returns expression (r)
dydx_3<-Deriv(y,'x')#Deriv() taking a function as input &returns you a function
dydx_3
dydx_3(2)
dydx_3(3)-eval({x=3;dydx_2})<0.00001#numerically the same
matplot(xbase,cbind(y(xbase),dydx_3(xbase)),type = 'l',
        xlab = 'x',ylab = 'y',main = 'y=x^2 and its 1st derivative')
abline(h=0,untf=F)
abline(v=0,untf =F)

#Have a look At all the expressions we have defined
ls()
var<-ls()
var
mget(var)
summary(mget(var))
class(summary(mget(var)))
var2<-as.data.frame(summary(mget(var)))
varType<-var2$Freq
varType
varExp<-var2[which(varType=='expression'),]
varExp
class(varExp)
varExpLst<-as.list(mget(as.vector(varExp$Var1)))
varExpLst
#Now that we FINALLY get a list of variables

#Let's write this as a function
#view_expression<-function(){
  #var<-ls(pos = .GlobalEnv)
 # var2<-as.data.frame(summary(mget(varpos = .GlobalEnv)))
 # varExpLst<-as.list(mget(as.vector(var2[which(var2$Freq=='expression'),]$Var1),pos = .GlobalEnv))
  #varExpLst
#}view_expression()
#Let's defign some more complexed functions
g<-expression(h*y_s/dydx_2+y_s*32*h)#in Rstudio blue Tab = function loaded
g
g<-Simplify(g)#introducing another function of package 'Deriv'
g
Simplify(expression(32500/100))
Simplify(expression(35*p-28*g*p))
Simplify(expression(e^(-n)*e^(28*(n/2))))
Simplify(expression(2*sin(x)*cos(x)/(cos(x))^2))

dgdh<-deriv(g,'h')#But Deriv is limited;e.g. not very capable of substitution
dgdh
D(g,'h')

#remove(xbase)
detach("package:Deriv", unload=TRUE)
#------------------------------------------------------
#try the more powerful Ryacas library here
#https://github.com/ggrothendieck/ryacas
#http://www.yacas.org
#library('Ryacas')
suppressPackageStartupMessages(library(Ryacas))

#The difference of Ryacas: why forcing the format to R is not a good idea
dydx_2<-deriv(y_s,'x')

y<-yacas(expression(x^2))
y
remove(x)
#instead of eval({x=0;y})you go
Eval(y,list(x = 0))#Eval E is in UPPER CASE
x#Ryacas  substitution does not work on global environment
#instead of D(y_s,'x')
dydx_4<-yacas('D(x)x^2')
dydx_4
yacas('Solve((%)==0,x)')#and you can't assign it to a normal kind of r object so far
Eval(y,list(x=0))
class(dydx_4)
mode(dydx_4)
summary(dydx_4)
dydx_4$text
dydx_4$OMForm
dydx_5<-parse(text = dydx_4$text)
dydx_5#So dydx_5 is an R expression which we just formatted, instead of an Ryacas expression
x<-(-4)
eval(dydx_5)

#yacas('Solve(dydx_4==0,x)')

#-----------------------------------------------------------
#BUT THIS IS NOT THE RECOMMENDED WAY OF USING IT!!!
#LET'S SEE HOW RYACAS FORMATS ITSELF, OR YACAS.
dydx_4<-Expand(dydx_4)
summary(dydx_4)#Note that Class1 Sym here? 
#Sym is a recommended way of defining terms in Ryacas
#Why not keeping everything in R?
#Because some functions or logics in simbolic calculations are essentially different
#from that of the numerical languages.
#If there is a systematic way of getting works done painlessly in Ryacas and it allows some outputs
#with compatibility to R in order to avoid copying and pasting, then why not use it?

#rm(list = ls())
detach("package:Ryacas", unload=TRUE)
