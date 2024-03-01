package com.dpw.runner.shipment.services.ReportingService.Models;

import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
public class OtherChargesResponse implements IDocumentModel{
    private List<String> otherChargesItems;
    private List<String> newOtherChargesItems;

    public void addOtherChargesItems(String value) {
        if(otherChargesItems == null) {
            otherChargesItems = new ArrayList<>();
        }
        otherChargesItems.add(value);
    }

    public void addNewOtherChargesItems(String value) {
        if(newOtherChargesItems == null) {
            newOtherChargesItems = new ArrayList<>();
        }
        newOtherChargesItems.add(value);
    }
}
