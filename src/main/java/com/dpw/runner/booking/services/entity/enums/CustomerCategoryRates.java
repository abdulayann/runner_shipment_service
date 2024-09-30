package com.dpw.runner.booking.services.entity.enums;

public enum CustomerCategoryRates {

    CATEGORY_1(1, "Category 1"),
    CATEGORY_2(2, "Category 2"),
    CATEGORY_3(3, "Category 3"),
    CATEGORY_4(4, "Category 4"),
    CATEGORY_5(5, "Category 5");
    private final int value;
    private final String description;

    CustomerCategoryRates(int value, String description) {
        this.value = value;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public int getValue() {
        return value;
    }

}
