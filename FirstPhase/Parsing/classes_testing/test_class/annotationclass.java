@SupportedAnnotationTypes("*")
@SupportedSourceVersion( SourceVersion.RELEASE_6 )
@ProhibitAnnotationProcessing
public class Processor
        extends AbstractProcessor
{
    @Override
    public boolean process( final Set<? extends TypeElement> annotations,
                            final RoundEnvironment env )
    {
        if( !env.processingOver() )
        {
            for( Element e : env.getRootElements() )
            {
                TypeElement te = findEnclosingTypeElement( e );
                System.out.printf( "\nScanning Type %s\n\n",
                                   te.getQualifiedName() );
                for( ExecutableElement ee : ElementFilter.methodsIn(
                        te.getEnclosedElements() ) )
                {
                    Action action = ee.getAnnotation( Action.class );
                    
                    System.out.printf(
                             "%s Action value = %s\n ",
                            ee.getSimpleName(),
                            action == null ? null : action.value() );
                }
            }
        }
        return false;
    }
    public static TypeElement findEnclosingTypeElement( Element e )
    {
        while( e != null && !(e instanceof TypeElement) )
        {
            e = e.getEnclosingElement();
        }
        return TypeElement.class.cast( e );
    }
}
