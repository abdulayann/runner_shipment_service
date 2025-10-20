package com.dpw.runner.shipment.services.kafka.dto.inttra;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
@Setter
public class Equipment implements Serializable {
    private Integer count;
    private Boolean identifierIsActual;
    private String identifierValue;
    private EquipmentSizeCode equipmentSizeCode;
    private String supplierType;
    private String serviceType;
    private String fullIndicator;
    private String netWeight;
    private String netVolume;
    private String airFlow;
    private String atmosphere;
    private Boolean nonActiveReefer;
    private String temperature;
    private String reeferHandling;
    private List<EquipmentReference> references;
    private List<String> comments;
    private String specialServices;
    private EquipmentOutOfGaugeDetails equipmentOutOfGaugeDetails;
    private Haulage haulage;
}