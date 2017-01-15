{
        Document doc = Jsoup.parse(source);

        Elements links = doc.getElementsByTag("a");
        //Elements days = doc.getElementsByTag("U");
        StringBuilder sb = new StringBuilder();

        List<Food> food;
        String day, text;

        for(Element e: links) {
            //String href = e.attr("href");
            text = e.text();

            //System.out.println("Link: " + href);
            sb.append(text).append("\n");
            food = new ArrayList();

            switch (text) {
                case "Lundi":
                case "Mardi":
                case "Mercredi":
                case "Jeudi":
                case "Vendredi":
                case "Samedi":
                case "Dimanche":
                    day = text;
                    break;
                default:
                    food.add(new Food(text));
                    break;
            }

            Menu menu = new Menu(DayMenu.MONDAY, Meal.DINNER, food);
            MenuLab.getInstance().addMenu(menu);

        }
        //System.out.println(sb.toString());

        MenuLab lab = MenuLab.getInstance();
    }
