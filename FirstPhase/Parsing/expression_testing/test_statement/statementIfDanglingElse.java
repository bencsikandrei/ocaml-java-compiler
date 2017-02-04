{
	if (door.isOpen())
		if (resident.isVisible())
			resident.greet("Hello!");
	else door.bell.ring();	// A “dangling else ”
}
