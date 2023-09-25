package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShippingInstructionModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;

public class ShippingInstructionReport extends IReport{

    @Override
    public Map<String, Object> getData(Long id) {
        ShippingInstructionModel shippingInstructionModel = (ShippingInstructionModel) getDocumentModel(id);
        return populateDictionary(shippingInstructionModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        ShippingInstructionModel shippingInstructionModel = new ShippingInstructionModel();
        shippingInstructionModel.shipmentDetails = getShipment(id);
        shippingInstructionModel.tenantDetails = getTenant(TenantContext.getCurrentTenant());
        return shippingInstructionModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ShippingInstructionModel shippingInstructionModel = (ShippingInstructionModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();

        List<String> consigner = null;
        if(shippingInstructionModel.shipmentDetails.getConsigner() != null) {
            consigner = getOrgAddressWithPhoneEmail(shippingInstructionModel.shipmentDetails.getConsigner());
            if(shippingInstructionModel.shipmentDetails.getConsigner().getOrgData() != null) {
                Map<String, Object> partyOrg = shippingInstructionModel.shipmentDetails.getConsigner().getOrgData();
                if(getValueFromMap(partyOrg, "FullName") != null) {
                    consigner.add(0, getValueFromMap(partyOrg, "FullName"));
                }
            }
        }

        List<String> consignee = null;
        if(shippingInstructionModel.shipmentDetails.getConsignee() != null) {
            consignee = getOrgAddressWithPhoneEmail(shippingInstructionModel.shipmentDetails.getConsignee());
            if(shippingInstructionModel.shipmentDetails.getConsignee().getOrgData() != null) {
                Map<String, Object> partyOrg = shippingInstructionModel.shipmentDetails.getConsignee().getOrgData();
                if(getValueFromMap(partyOrg, "FullName") != null) {
                    consignee.add(0, getValueFromMap(partyOrg, "FullName"));
                }
            }
        }

        List<String> notify = null;
        if(shippingInstructionModel.shipmentDetails.getAdditionalDetails().getNotifyParty() != null) {
            notify = getOrgAddressWithPhoneEmail(shippingInstructionModel.shipmentDetails.getAdditionalDetails().getNotifyParty());
            if(shippingInstructionModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData() != null) {
                Map<String, Object> partyOrg = shippingInstructionModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData();
                if(getValueFromMap(partyOrg, "FullName") != null) {
                    notify.add(0, getValueFromMap(partyOrg, "FullName"));
                }
            }
        }

        dictionary.put(ReportConstants.SHIPPER_ADDRESS, consigner);
        dictionary.put(ReportConstants.CONSIGNEE_ADDRESS, consignee);
        dictionary.put(ReportConstants.NOTIFY_ADDRESS, notify);
        dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, notify);
        dictionary.put(ReportConstants.CONSIGNEE_FREETEXT, consignee);
        dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
        dictionary.put(ReportConstants.MBL_NUMBER, shippingInstructionModel.shipmentDetails.getMasterBill());
        dictionary.put(ReportConstants.HBL_NUMBER, shippingInstructionModel.shipmentDetails.getHouseBill());
        if(shippingInstructionModel.shipmentDetails.getCarrierDetails().getOrigin() != null) {
            UnlocationsResponse origin = getUNLocRow(shippingInstructionModel.shipmentDetails.getCarrierDetails().getOrigin());
            if(origin != null)
                dictionary.put(ReportConstants.POR, origin.getNameWoDiacritics());
        }
        dictionary.put(ReportConstants.POL, getPortDetails(shippingInstructionModel.shipmentDetails.getCarrierDetails().getOriginPort()));
        dictionary.put(ReportConstants.POD, getPortDetails(shippingInstructionModel.shipmentDetails.getCarrierDetails().getDestinationPort()));
        dictionary.put(ReportConstants.POFD, getPortDetails(shippingInstructionModel.shipmentDetails.getCarrierDetails().getDestination()));
        dictionary.put(ReportConstants.PO_DELIVERY, getPortDetails(shippingInstructionModel.shipmentDetails.getCarrierDetails().getDestinationPort()));
        dictionary.put(ReportConstants.SERVICE_TYPE, shippingInstructionModel.shipmentDetails.getTransportMode());
        dictionary.put(ReportConstants.PPCC, shippingInstructionModel.shipmentDetails.getPaymentTerms());
        dictionary.put(ReportConstants.CURRENT_DATE, IReport.ConvertToDPWDateFormat(LocalDateTime.now()));
        if(shippingInstructionModel.shipmentDetails.getCarrierDetails().getEtd() != null)
            dictionary.put(ReportConstants.ETD, shippingInstructionModel.shipmentDetails.getCarrierDetails().getEtd().format(DateTimeFormatter.ofPattern("dd/MMM/y")));
        else
            dictionary.put(ReportConstants.ETD, null);
        if(shippingInstructionModel.shipmentDetails.getCarrierDetails().getEta() != null)
            dictionary.put(ReportConstants.ETA, shippingInstructionModel.shipmentDetails.getCarrierDetails().getEta().format(DateTimeFormatter.ofPattern("dd/MMM/y")));
        else
            dictionary.put(ReportConstants.ETA, null);
        dictionary.put(ReportConstants.CARRIER, shippingInstructionModel.shipmentDetails.getCarrierDetails().getShippingLine());
        dictionary.put(ReportConstants.VESSEL_NAME, shippingInstructionModel.shipmentDetails.getCarrierDetails().getVessel());
        dictionary.put(ReportConstants.VOYAGE, shippingInstructionModel.shipmentDetails.getCarrierDetails().getVoyage());
        dictionary.put(ReportConstants.Flight_Number, shippingInstructionModel.shipmentDetails.getCarrierDetails().getFlightNumber());
        dictionary.put(ReportConstants.JOB_NUMBER, shippingInstructionModel.shipmentDetails.getShipmentId());
        dictionary.put(ReportConstants.TRANSPORT_MODE, shippingInstructionModel.shipmentDetails.getTransportMode());
        dictionary.put(ReportConstants.SHIPMENT_TYPE, shippingInstructionModel.shipmentDetails.getDirection());
//        dictionary.put(ReportConstants.SUMMARY, shippingInstructionModel.shipmentDetails.getAdditionalDetails().getSummary()); // TODO- which field is summary

        long totalPacks = 0;
        double totalVolume = 0;
        String unitOfTotalVolume = null;
        double totalWeight = 0;
        String unitOfTotalWeight = null;
        boolean breakFlagVolume = false;
        boolean breakFlagWeight = false;

        if(shippingInstructionModel.shipmentDetails.getPackingList() != null && shippingInstructionModel.shipmentDetails.getPackingList().size() > 0) {
            // TODO- Implementation
        }

        if(totalPacks != 0)
            dictionary.put(ReportConstants.TOTAL_PACKS, totalPacks);
        else
            dictionary.put(ReportConstants.TOTAL_PACKS, null);

        if(breakFlagVolume || totalVolume == 0) {
            dictionary.put(ReportConstants.TOTAL_VOLUME, null);
            dictionary.put(ReportConstants.UOTV, null);
        }
        else {
            dictionary.put(ReportConstants.TOTAL_VOLUME, twoDecimalPlacesFormat(BigDecimal.valueOf(totalVolume).toString()));
            dictionary.put(ReportConstants.UOTV, unitOfTotalVolume);
        }

        if(breakFlagWeight || totalWeight == 0) {
            dictionary.put(ReportConstants.TOTAL_WEIGHT, null);
            dictionary.put(ReportConstants.UOTW, null);
        }
        else {
            dictionary.put(ReportConstants.TOTAL_WEIGHT, twoDecimalPlacesFormat(BigDecimal.valueOf(totalWeight).toString()));
            dictionary.put(ReportConstants.UOTW, unitOfTotalWeight);
        }

        dictionary.put(ReportConstants.AGENT, getListOfStrings(shippingInstructionModel.tenantDetails.tenantName, shippingInstructionModel.tenantDetails.address1,
                        shippingInstructionModel.tenantDetails.address2, shippingInstructionModel.tenantDetails.city, shippingInstructionModel.tenantDetails.email,
                        shippingInstructionModel.tenantDetails.phone, shippingInstructionModel.tenantDetails.zipPostCode, shippingInstructionModel.tenantDetails.state));
        addTenantDetails(dictionary, shippingInstructionModel.tenantDetails);
        List<String> tenantsDataList = getListOfStrings(shippingInstructionModel.tenantDetails.tenantName, shippingInstructionModel.tenantDetails.address1, shippingInstructionModel.tenantDetails.address2,
                shippingInstructionModel.tenantDetails.city, shippingInstructionModel.tenantDetails.state, shippingInstructionModel.tenantDetails.zipPostCode, shippingInstructionModel.tenantDetails.country,
                shippingInstructionModel.tenantDetails.email, shippingInstructionModel.tenantDetails.websiteUrl, shippingInstructionModel.tenantDetails.phone);
        if(tenantsDataList != null)
            dictionary.put(ReportConstants.TENANT, tenantsDataList);
        addBLDetails(dictionary, shippingInstructionModel.shipmentDetails.getId());
        return dictionary;
    }

}
