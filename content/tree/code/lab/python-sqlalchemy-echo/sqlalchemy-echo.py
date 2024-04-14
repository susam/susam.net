from sqlalchemy import (
    Engine,
    ForeignKey,
    Integer,
    ScalarResult,
    String,
    create_engine,
    select,
)
from sqlalchemy.orm import (
    DeclarativeBase,
    Mapped,
    Session,
    mapped_column,
    relationship,
)


class Base(DeclarativeBase):
    pass


class Person(Base):
    """Name of a person and city they live in."""

    __tablename__ = "person"
    serial: Mapped[int] = mapped_column(Integer, primary_key=True, nullable=False)
    name: Mapped[str] = mapped_column(String, nullable=False)
    city: Mapped[str] = mapped_column(String, nullable=False)
    books: Mapped[list["Book"]] = relationship("Book")


class Book(Base):
    """Books borrowed by each person."""

    __tablename__ = "book"
    serial: Mapped[int] = mapped_column(Integer, primary_key=True, nullable=False)
    title: Mapped[str] = mapped_column(String, nullable=False)
    author: Mapped[str] = mapped_column(String, nullable=False)
    person: Mapped[int] = mapped_column(
        Integer, ForeignKey("person.serial"), nullable=False
    )


def main() -> None:
    engine: Engine = create_engine("sqlite:///library.db", echo=True)
    Base.metadata.create_all(engine)

    with Session(engine) as session, session.begin():
        person: Person = Person(name="Alice", city="London")
        person.books.append(Book(title="The Code Book", author="Simon Singh"))
        person.books.append(Book(title="The Calculus Wars", author="Jason Bardi"))
        session.add(person)

    with Session(engine) as session, session.begin():
        persons: ScalarResult[Person] = session.scalars(
            select(Person).where(Person.name == "Alice")
        )
        for person in persons:
            for book in person.books:
                print(f"{person.name}, {person.city}, {book.title}, {book.author}")


if __name__ == "__main__":
    main()
