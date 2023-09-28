package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.TaxPair;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentCANModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;

@Component
public class ShipmentCANReport extends IReport {

    @Autowired
    private JsonHelper jsonHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        ShipmentCANModel shipmentCANModel = (ShipmentCANModel) getDocumentModel(id);
        return populateDictionary(shipmentCANModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        ShipmentCANModel shipmentCANModel = new ShipmentCANModel();
        shipmentCANModel.shipmentDetails = getShipment(id);
        shipmentCANModel.tenantModel = getTenant(TenantContext.getCurrentTenant());
        shipmentCANModel.consolidationModel = getFirstConsolidationFromShipmentId(id);
        shipmentCANModel.shipmentSettingsDetails = getShipmentSettings(TenantContext.getCurrentTenant());
        return shipmentCANModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ShipmentCANModel shipmentCANModel = (ShipmentCANModel) documentModel;
        Map<String, Object> dictionary = new HashMap<>();
        // TODO- ShipmentHouseBL.isHBL = getIsHBL(shipmentId);
        // TODO- dictionary = ShipmentHouseBL.GetData();
        // List<BillCharges> billChargesList = new ArrayList<>();
        TaxPair<String, String> tax1 = new TaxPair<>("TaxType1", "0");
        TaxPair<String, String> tax2 = new TaxPair<>("TaxType2", "0");
        TaxPair<String, String> tax3 = new TaxPair<>("TaxType3", "0");
        TaxPair<String, String> tax4 = new TaxPair<>("TaxType4", "0");
        List<TaxPair<String, String>> taxes = new ArrayList<>();
        taxes.add(tax1);
        taxes.add(tax2);
        taxes.add(tax3);
        taxes.add(tax4);
        double totalTax = 0.0;
        double totalBillAmount = 0.0;
        double taxValue0 = 0.0;
        double taxValue1 = 0.0;
        double taxValue2 = 0.0;
        double taxValue3 = 0.0;
        // TODO- billRows fetch and calculate tax values
        if(shipmentCANModel.shipmentSettingsDetails != null && true) { // TODO- GSTTaxAutoCalculation flag check
            for (TaxPair<String, String> tax : taxes) {
                if(tax.getTaxType() == "TaxType1")
                    tax.setTaxType("SGST");
                if(tax.getTaxType() == "TaxType2")
                    tax.setTaxType("CGST");
                if(tax.getTaxType() == "TaxType3")
                    tax.setTaxType("UGST");
                if(tax.getTaxType() == "TaxType4")
                    tax.setTaxType("IGST");
            }
        }
        if(taxes != null) {
            taxes.get(0).setTaxValue(twoDecimalPlacesFormatDecimal(BigDecimal.valueOf(taxValue0)));
            taxes.get(1).setTaxValue(twoDecimalPlacesFormatDecimal(BigDecimal.valueOf(taxValue1)));
            taxes.get(2).setTaxValue(twoDecimalPlacesFormatDecimal(BigDecimal.valueOf(taxValue2)));
            taxes.get(3).setTaxValue(twoDecimalPlacesFormatDecimal(BigDecimal.valueOf(taxValue3)));
        }
        dictionary.put(ReportConstants.IGM_NO, shipmentCANModel.shipmentDetails.getAdditionalDetails().getIGMFileNo());
        dictionary.put(ReportConstants.FLIGHT_NAME, shipmentCANModel.shipmentDetails.getCarrierDetails().getShippingLine());
        dictionary.put(ReportConstants.FLIGHT_NUMBER, shipmentCANModel.shipmentDetails.getCarrierDetails().getFlightNumber());
        dictionary.put(MBL_NUMBER, shipmentCANModel.shipmentDetails.getMasterBill());
        dictionary.put(HBL_NUMBER, shipmentCANModel.shipmentDetails.getHouseBill());
        dictionary.put(DESC_OF_GOODS, shipmentCANModel.shipmentDetails.getGoodsDescription());
        dictionary.put(JOB_NO, shipmentCANModel.shipmentDetails.getShipmentId());
        if(shipmentCANModel.shipmentDetails != null && shipmentCANModel.shipmentDetails.getFreightLocal() != null)
            dictionary.put(ReportConstants.FREIGHT_LOCAL, shipmentCANModel.shipmentDetails.getFreightLocal());
        if(shipmentCANModel.shipmentDetails != null && shipmentCANModel.shipmentDetails.getFreightLocalCurrency() != null && !shipmentCANModel.shipmentDetails.getFreightLocalCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_LOCAL_CURRENCY, shipmentCANModel.shipmentDetails.getFreightLocalCurrency());
        if(shipmentCANModel.shipmentDetails != null && shipmentCANModel.shipmentDetails.getFreightOverseas() != null)
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS, shipmentCANModel.shipmentDetails.getFreightOverseas());
        if(shipmentCANModel.shipmentDetails != null && shipmentCANModel.shipmentDetails.getFreightOverseasCurrency() != null && !shipmentCANModel.shipmentDetails.getFreightOverseasCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS_CURRENCY, shipmentCANModel.shipmentDetails.getFreightOverseasCurrency());
        // TODO- fetch consolidation address row and then populate dictionary accordingly
        if(shipmentCANModel.shipmentSettingsDetails != null && true) // TODO- check for EnableIGMDetails in tenant settings
        {
            if(shipmentCANModel.shipmentDetails.getDirection() != null && shipmentCANModel.shipmentDetails.getDirection() == Constants.IMP) {
                if(shipmentCANModel.shipmentDetails.getAdditionalDetails().getIGMFileDate() != null) {
                    dictionary.put(ReportConstants.IGM_FILE_DATE, shipmentCANModel.shipmentDetails.getAdditionalDetails().getIGMFileDate());
                }
                if(shipmentCANModel.shipmentDetails.getAdditionalDetails().getIGMFileNo() != null) {
                    dictionary.put(ReportConstants.IGM_FILE_NO, shipmentCANModel.shipmentDetails.getAdditionalDetails().getIGMFileNo());
                }
                if(shipmentCANModel.shipmentDetails.getAdditionalDetails().getIGMInwardDate() != null) {
                    dictionary.put(ReportConstants.IGM_INWARD_DATE, shipmentCANModel.shipmentDetails.getAdditionalDetails().getIGMInwardDate());
                }
                if(shipmentCANModel.shipmentDetails.getAdditionalDetails().getInwardDateAndTime() != null) {
                    dictionary.put(ReportConstants.INWARD_DATE_TIME, shipmentCANModel.shipmentDetails.getAdditionalDetails().getInwardDateAndTime());
                }
                if(shipmentCANModel.shipmentDetails.getAdditionalDetails().getLineNumber() != null) {
                    dictionary.put(ReportConstants.LINE_NUMBER, shipmentCANModel.shipmentDetails.getAdditionalDetails().getLineNumber());
                }
                if(shipmentCANModel.shipmentDetails.getAdditionalDetails().getSubLineNumber() != null) {
                    dictionary.put(ReportConstants.SUB_LINE_NUMBER, shipmentCANModel.shipmentDetails.getAdditionalDetails().getSubLineNumber());
                }
                if(shipmentCANModel.shipmentDetails.getAdditionalDetails().getIsInland()) {
                    dictionary.put(ReportConstants.IS_INLAND, shipmentCANModel.shipmentDetails.getAdditionalDetails().getIsInland()?"Yes":"No");
                    if(shipmentCANModel.shipmentDetails.getAdditionalDetails().getSMTPIGMDate() != null) {
                        dictionary.put(ReportConstants.SMTPIGM_DATE, shipmentCANModel.shipmentDetails.getAdditionalDetails().getSMTPIGMDate());
                    }
                    if(shipmentCANModel.shipmentDetails.getAdditionalDetails().getSMTPIGMNumber() != null) {
                        dictionary.put(ReportConstants.SMTPIGM_NUMBER, shipmentCANModel.shipmentDetails.getAdditionalDetails().getSMTPIGMNumber());
                    }
                    if(shipmentCANModel.shipmentDetails.getAdditionalDetails().getLocalLineNumber() != null) {
                        dictionary.put(ReportConstants.LOCAL_LINE_NUMBER, shipmentCANModel.shipmentDetails.getAdditionalDetails().getLocalLineNumber());
                    }
                }
            }
        }
        // TODO- PortofLoading, etc. master data dictionary population 6 fields
        dictionary.put(PLACE_OF_RECEIPT, shipmentCANModel.shipmentDetails.getCarrierDetails().getOrigin());
        dictionary.put(PLACE_OF_DELIVERY, shipmentCANModel.shipmentDetails.getCarrierDetails().getDestination());
        dictionary.put(TRANSPORT_MODE, shipmentCANModel.shipmentDetails.getTransportMode());
        dictionary.put(JOB_NO, shipmentCANModel.shipmentDetails.getShipmentId());
        dictionary.put(ReportConstants.CLIENT, getValueFromMap(shipmentCANModel.shipmentDetails.getClient().getOrgData(), "FullName"));
        dictionary.put(AIRLINE, shipmentCANModel.shipmentDetails.getCarrierDetails().getShippingLine());
        dictionary.put(CMS_REMARKS, shipmentCANModel.shipmentDetails.getGoodsDescription());
        if(shipmentCANModel.shipmentDetails.getVolumetricWeight() != null)
            dictionary.put(ReportConstants.V_WEIGHT_AND_UNIT, String.format("%.2f %s", twoDecimalPlacesFormatDecimal(shipmentCANModel.shipmentDetails.getVolumetricWeight()), shipmentCANModel.shipmentDetails.getVolumetricWeightUnit()));
        if(shipmentCANModel.shipmentDetails.getWeight() != null)
            dictionary.put(ReportConstants.WEIGHT_AND_UNIT, String.format("%.2f %s", twoDecimalPlacesFormatDecimal(shipmentCANModel.shipmentDetails.getWeight()), shipmentCANModel.shipmentDetails.getWeightUnit()));
        if(shipmentCANModel.shipmentDetails.getVolume() != null)
            dictionary.put(ReportConstants.VOLUME_AND_UNIT, String.format("%.2f %s", twoDecimalPlacesFormatDecimal(shipmentCANModel.shipmentDetails.getVolume()), shipmentCANModel.shipmentDetails.getVolumeUnit()));
        dictionary.put(ReportConstants.MARKS_AND_NUMBER, shipmentCANModel.shipmentDetails.getMarksNum());
        dictionary.put(ReportConstants.NO_OF_PACKAGES, shipmentCANModel.shipmentDetails.getNoOfPacks());
        if(shipmentCANModel.consolidationModel != null && shipmentCANModel.consolidationModel.getPayment() != null) {
            dictionary.put(FREIGHT, shipmentCANModel.consolidationModel.getPayment());
        }
        // TODO- Bill charges and taxes/amounts calculations...
        dictionary.put(ReportConstants.TOTAL_BILL_AMOUNT, twoDecimalPlacesFormatDecimal(BigDecimal.valueOf(totalBillAmount)));
        dictionary.put(TAXES, taxes);
        dictionary.put(TOTAL_TAX_AMOUNT, twoDecimalPlacesFormatDecimal(BigDecimal.valueOf(totalTax)));
        return dictionary;
    }
}
