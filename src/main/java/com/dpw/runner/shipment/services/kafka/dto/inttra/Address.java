package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
public class Address implements Serializable {
    private String unstructuredAddress01;
    private String unstructuredAddress02;
    private String unstructuredAddress03;
    private String unstructuredAddress04;
    private String street01;
    private String street02;
    private String city;
    private String state;
    private String postalCode;
    private Country country;
}