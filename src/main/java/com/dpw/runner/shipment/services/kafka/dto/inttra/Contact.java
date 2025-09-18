package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
public class Contact implements Serializable {
    private String name;
    private String contactType;
    private String title;
    private String department;
    private List<String> phones;
    private List<String> faxes;
    private List<String> emails;
    private Address address;
}
