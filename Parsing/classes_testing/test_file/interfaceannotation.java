@Retention(RetentionPolicy.SOURCE)
@Target(ElementType.TYPE)
public @interface Action {}

@Retention(RetentionPolicy.SOURCE)
@Target({ElementType.TYPE, ElementType.FIELD, ElementType.METHOD})
public @interface ActionReference {}

@Retention(RetentionPolicy.SOURCE)
@Target({ElementType.TYPE, ElementType.FIELD, ElementType.METHOD, ElementType.PACKAGE})
public @interface ActionReferences {
ActionReference[] value();
}