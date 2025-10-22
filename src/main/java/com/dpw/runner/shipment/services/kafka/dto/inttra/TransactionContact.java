package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
public class TransactionContact implements Serializable {
    private String name;
    private String contactType;
    private List<String> phones;
    private List<String> faxes;
    private List<String> emails;
}
