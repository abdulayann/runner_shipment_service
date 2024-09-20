package com.dpw.runner.shipment.services.ReportingService.Models;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class OtherChargesResponse implements IDocumentModel{
    private List<String> otherChargesItems;
    private List<String> newOtherChargesItems;
}
