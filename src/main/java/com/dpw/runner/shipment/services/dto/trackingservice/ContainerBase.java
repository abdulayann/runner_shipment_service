package com.dpw.runner.shipment.services.dto.trackingservice;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ContainerBase {

    private String type;
    private String size;
    private String typeIsoCode;
    private Object bolNumber;
    private Object bookingNumber;
    private String clientBookingNumber;
    private String shipmentReference;
    private Object sealNumber;
    private Object marks;
    private Object incoterm;
    private Object shipper;
    private Object consignee;
    private String weight;
    private String weightUom;
    private String numberOfPackages;
    private String packageType;
    private Object reeferTemperature;
    private Object commodity;
    private Object latitude;
    private Object longitude;
    private Object location;
    private Object locationUpdateTime;

}
