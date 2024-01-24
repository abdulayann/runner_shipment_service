package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.TaxPair;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentCANModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.BillChargesResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillingResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;

@Component
public class ShipmentCANReport extends IReport {

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private HblReport hblReport;

    @Autowired
    private IHblDao hblDao;

    @Override
    public Map<String, Object> getData(Long id) {
        ShipmentCANModel shipmentCANModel = (ShipmentCANModel) getDocumentModel(id);
        return populateDictionary(shipmentCANModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        ShipmentCANModel shipmentCANModel = new ShipmentCANModel();
            List<Hbl> hblList = hblDao.findByShipmentId(id);
            if(hblList == null || hblList.size() == 0)
                throw new ValidationException("Bl Object not found!");
            shipmentCANModel.shipmentDetails = getShipment(id);
            shipmentCANModel.tenantDetails = getTenant();
            shipmentCANModel.consolidationModel = getFirstConsolidationFromShipmentId(id);
            shipmentCANModel.shipmentSettingsDetails = getShipmentSettings(TenantContext.getCurrentTenant());
            shipmentCANModel.tenantSettingsResponse = getTenantSettings();
            shipmentCANModel.isHBL = getIsHbl(shipmentCANModel.shipmentDetails);
            return shipmentCANModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ShipmentCANModel shipmentCANModel = (ShipmentCANModel) documentModel;
        Map<String, Object> dictionary = hblReport.getData(shipmentCANModel.shipmentDetails.getId());
        populateShipmentOrganizationsLL(shipmentCANModel.shipmentDetails, dictionary);
        List<BillChargesResponse> allBillCharges = new ArrayList<>();
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
        List<BillingResponse> billingsList = getBillingData(shipmentCANModel.shipmentDetails.getGuid());
        if(billingsList != null && billingsList.size() > 0) {
            for (BillingResponse bill: billingsList) {
                List<BillChargesResponse> billChargesList = getBillChargesData(bill.getGuid());
                if(billChargesList != null && billChargesList.size() > 0) {
                    for (BillChargesResponse billCharge : billChargesList) {
                        allBillCharges.add(billCharge);
                        BigDecimal overseasSellAmount = billCharge.getOverseasSellAmount() != null ? billCharge.getOverseasSellAmount() : BigDecimal.ZERO;
                        BigDecimal overseasTax = billCharge.getOverseasTax() != null ? billCharge.getOverseasTax() : BigDecimal.ZERO;
                        totalBillAmount = totalBillAmount + overseasSellAmount.doubleValue() + overseasTax.doubleValue();
                        totalTax = totalTax + overseasTax.doubleValue();

                        BigDecimal exchRate = billCharge.getSellExchange() != null ? billCharge.getSellExchange() : BigDecimal.ZERO;
                        taxValue0 = taxValue0 + (billCharge.getTaxType1() != null ? billCharge.getTaxType1() : BigDecimal.ZERO).multiply(exchRate).doubleValue();
                        taxValue1 = taxValue1 + (billCharge.getTaxType2() != null ? billCharge.getTaxType2() : BigDecimal.ZERO).multiply(exchRate).doubleValue();
                        taxValue2 = taxValue2 + (billCharge.getTaxType3() != null ? billCharge.getTaxType3() : BigDecimal.ZERO).multiply(exchRate).doubleValue();
                        taxValue3 = taxValue3 + (billCharge.getTaxType4() != null ? billCharge.getTaxType4() : BigDecimal.ZERO).multiply(exchRate).doubleValue();
                    }
                }
            }
        }
        if(shipmentCANModel.tenantSettingsResponse != null && shipmentCANModel.tenantSettingsResponse.isGSTTaxAutoCalculation()) {
            for (TaxPair<String, String> tax : taxes) {
                if(tax.getTaxType().equalsIgnoreCase("TaxType1"))
                    tax.setTaxType("SGST");
                if(tax.getTaxType().equalsIgnoreCase("TaxType2"))
                    tax.setTaxType("CGST");
                if(tax.getTaxType().equalsIgnoreCase("TaxType3"))
                    tax.setTaxType("UGST");
                if(tax.getTaxType().equalsIgnoreCase("TaxType4"))
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
        if(shipmentCANModel.shipmentDetails.getShipmentAddresses() != null && shipmentCANModel.shipmentDetails.getShipmentAddresses().size() > 0) {
            for (PartiesModel shipmentAddress: shipmentCANModel.shipmentDetails.getShipmentAddresses()) {
                if(shipmentAddress.getType() == CUSTOM_HOUSE_AGENT && shipmentAddress.getOrgData() != null && getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME) != null) {
                    dictionary.put(CHAPartyDescription, getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME));
                }
            }
        }
        if(shipmentCANModel.tenantSettingsResponse != null && shipmentCANModel.tenantSettingsResponse.isEnableIGMDetails())
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
        if(shipmentCANModel.shipmentDetails.getCarrierDetails().getOriginPort() != null) {
            UnlocationsResponse pol = getUNLocRow(shipmentCANModel.shipmentDetails.getCarrierDetails().getOriginPort());
            if(pol != null) {
                if(pol.getPortName() != null)
                    dictionary.put(PORT_OF_LOADING, pol.getPortName());
                if(pol.getCountry() != null)
                    dictionary.put(PORT_OF_LOADING_COUNTRY, pol.getCountry());
            }
        }
        if(shipmentCANModel.shipmentDetails.getCarrierDetails().getDestinationPort() != null) {
            UnlocationsResponse pod = getUNLocRow(shipmentCANModel.shipmentDetails.getCarrierDetails().getDestinationPort());
            if(pod != null) {
                if(pod.getPortName() != null)
                {
                    dictionary.put(PORT_OF_DISCHARGE, pod.getPortName());
                    dictionary.put(PORT_OF_FINAL_DESTINATION, pod.getPortName());
                }
                if(pod.getCountry() != null) {
                    dictionary.put(PORT_OF_DISCHARGE_COUNTRY, pod.getCountry());
                    dictionary.put(PORT_OF_FINAL_DESTINATION_COUNTRY, pod.getCountry());
                }
            }
        }
        dictionary.put(PLACE_OF_RECEIPT, shipmentCANModel.shipmentDetails.getCarrierDetails().getOrigin());
        dictionary.put(PLACE_OF_DELIVERY, shipmentCANModel.shipmentDetails.getCarrierDetails().getDestination());
        dictionary.put(TRANSPORT_MODE, shipmentCANModel.shipmentDetails.getTransportMode());
        dictionary.put(JOB_NO, shipmentCANModel.shipmentDetails.getShipmentId());
        dictionary.put(ReportConstants.CLIENT, getValueFromMap(shipmentCANModel.shipmentDetails.getClient().getOrgData(), "FullName"));
        dictionary.put(AIRLINE, shipmentCANModel.shipmentDetails.getCarrierDetails().getShippingLine());
        dictionary.put(CMS_REMARKS, shipmentCANModel.shipmentDetails.getGoodsDescription());
        if(shipmentCANModel.shipmentDetails.getVolumetricWeight() != null)
            dictionary.put(ReportConstants.V_WEIGHT_AND_UNIT, String.format("%s %s", twoDecimalPlacesFormatDecimal(shipmentCANModel.shipmentDetails.getVolumetricWeight()), shipmentCANModel.shipmentDetails.getVolumetricWeightUnit()));
        if(shipmentCANModel.shipmentDetails.getWeight() != null)
            dictionary.put(ReportConstants.WEIGHT_AND_UNIT, String.format("%s %s", twoDecimalPlacesFormatDecimal(shipmentCANModel.shipmentDetails.getWeight()), shipmentCANModel.shipmentDetails.getWeightUnit()));
        if(shipmentCANModel.shipmentDetails.getVolume() != null)
            dictionary.put(ReportConstants.VOLUME_AND_UNIT, String.format("%s %s", twoDecimalPlacesFormatDecimal(shipmentCANModel.shipmentDetails.getVolume()), shipmentCANModel.shipmentDetails.getVolumeUnit()));
        dictionary.put(ReportConstants.MARKS_AND_NUMBER, shipmentCANModel.shipmentDetails.getMarksNum());
        dictionary.put(ReportConstants.NO_OF_PACKAGES, ReportHelper.addCommaWithoutDecimal(BigDecimal.valueOf(shipmentCANModel.shipmentDetails.getNoOfPacks())));
        if(shipmentCANModel.consolidationModel != null && shipmentCANModel.consolidationModel.getPayment() != null) {
            dictionary.put(FREIGHT, shipmentCANModel.consolidationModel.getPayment());
        }
        if(allBillCharges.size() > 0) {
            List<Map<String, Object>> billChargesDict = new ArrayList<>();
            for (BillChargesResponse billChargesResponse : allBillCharges) {
                String billChargeJson = jsonHelper.convertToJson(billChargesResponse);
                billChargesDict.add(jsonHelper.convertJsonToMap(billChargeJson));
            }
            for (Map<String, Object> v: billChargesDict) {
                v.put(TOTAL_AMOUNT, v.containsKey(OVERSEAS_SELL_AMOUNT) && v.get(OVERSEAS_SELL_AMOUNT) != null ? new BigDecimal(v.get(OVERSEAS_SELL_AMOUNT).toString()) : 0);
                v.put(TOTAL_AMOUNT, new BigDecimal(v.get(TOTAL_AMOUNT).toString()).add(v.containsKey(OVERSEAS_TAX) && v.get(OVERSEAS_TAX) != null ? new BigDecimal(v.get(OVERSEAS_TAX).toString()) : BigDecimal.ZERO));
                if(shipmentCANModel.tenantSettingsResponse != null && (shipmentCANModel.tenantSettingsResponse.getUseV2ScreenForBillCharges())) {
                    if (v.containsKey(REVENUE_MEASUREMENT_BASIS) && v.get(REVENUE_MEASUREMENT_BASIS) != null) {
                        v.put(REVENUE_MEASUREMENT_UNIT, MeasurementBasis.getByValue(Integer.parseInt(v.get(REVENUE_MEASUREMENT_BASIS).toString())));
                    }
                    if (v.containsKey(REVENUE_CALCULATED_VALUE) && v.get(REVENUE_CALCULATED_VALUE) != null) {
                        String[] split = v.get(REVENUE_CALCULATED_VALUE).toString().split(" ");
                        BigDecimal count = BigDecimal.ONE;
                        if (split != null && !split[0].equals("null"))
                            count = new BigDecimal(split[0]);
                        v.put(REVENUE_TOTAL_UNIT_COUNT, twoDecimalPlacesFormatDecimal(count));
                    }
                }
                else if(v.containsKey(MEASUREMENT_BASIS) && v.get(MEASUREMENT_BASIS) != null) {
                    v.put(MEASUREMENT_UNIT, MeasurementBasis.getByValue(Integer.parseInt(v.get(MEASUREMENT_BASIS).toString())));
                    if(v.containsKey(CALCULATED_VALUE) && v.get(CALCULATED_VALUE) != null) {
                        String[] split = v.get(CALCULATED_VALUE).toString().split(" ");
                        BigDecimal count = BigDecimal.ONE;
                        if (split != null && !split[0].equals("null"))
                            count = new BigDecimal(split[0]);
                        v.put(TOTAL_UNIT_COUNT, twoDecimalPlacesFormatDecimal(count));
                    }
                }

                if(v.containsKey(SELL_EXCHANGE) && v.get(SELL_EXCHANGE) != null)
                    v.put(SELL_EXCHANGE, twoDecimalPlacesFormat(v.get(SELL_EXCHANGE).toString()));
                if(v.containsKey(CURRENT_SELL_RATE) && v.get(CURRENT_SELL_RATE) != null)
                    v.put(CURRENT_SELL_RATE, twoDecimalPlacesFormat(v.get(CURRENT_SELL_RATE).toString()));
                if(v.containsKey(OVERSEAS_SELL_AMOUNT) && v.get(OVERSEAS_SELL_AMOUNT) != null)
                    v.put(OVERSEAS_SELL_AMOUNT, twoDecimalPlacesFormat(v.get(OVERSEAS_SELL_AMOUNT).toString()));
                if(v.containsKey(OVERSEAS_TAX) && v.get(OVERSEAS_TAX) != null)
                    v.put(OVERSEAS_TAX, twoDecimalPlacesFormat(v.get(OVERSEAS_TAX).toString()));
                if(v.containsKey(TAX_PERCENTAGE) && v.get(TAX_PERCENTAGE) != null)
                    v.put(TAX_PERCENTAGE, twoDecimalPlacesFormat(v.get(TAX_PERCENTAGE).toString()));
                if(v.containsKey(TOTAL_AMOUNT) && v.get(TOTAL_AMOUNT) != null)
                    v.put(TOTAL_AMOUNT, twoDecimalPlacesFormat(v.get(TOTAL_AMOUNT).toString()));
                if(v.containsKey(CHARGE_TYPE_CODE) && v.get(CHARGE_TYPE_CODE) != null) {
                    v.put(CHARGE_TYPE_DESCRIPTION_LL, GetChargeTypeDescriptionLL((String)v.get(CHARGE_TYPE_CODE)));
                }
            }
            dictionary.put(BILL_CHARGES, billChargesDict);
        }
        dictionary.put(ReportConstants.TOTAL_BILL_AMOUNT, twoDecimalPlacesFormatDecimal(BigDecimal.valueOf(totalBillAmount)));
        dictionary.put(TAXES, taxes);
        dictionary.put(TOTAL_TAX_AMOUNT, twoDecimalPlacesFormatDecimal(BigDecimal.valueOf(totalTax)));
        return dictionary;
    }
}
