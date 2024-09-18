package com.dpw.runner.shipment.services.utils;


import java.util.Objects;

public class WeightConverter {

    public static double convertToKg(double values, String from) {
        if (Objects.isNull(from))
            return values;
        from = from.toUpperCase();
        switch (from) {
            case "DT":
                return values * 100;
            case "HG":
                return values * 0.1;
            case "KT":
                return values * 1000000;
            case "LT":
                return values * 0.37;
            case "MG":
                return values / 1000000;
            case "G":
                return values / 1000;
            case "KG":
                return values * 1;
            case "LB":
                return values * 0.453592;
            case "MC":
                return values * 0.0002;
            case "OT":
                return values * 0.031104;
            case "MT":
                return values * 1000;
        }
        throw new RuntimeException(String.format("WeightUnit %s not found", from));
    }

    public static double round(double value, int places) {
        if (places < 0) throw new IllegalArgumentException();

        long factor = (long) Math.pow(10, places);
        value = value * factor;
        long tmp = Math.round(value);
        return (double) tmp / factor;
    }

}
