package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIRLINE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BILL_CHARGES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CALCULATED_VALUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHAPartyDescription;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGE_TYPE_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGE_TYPE_DESCRIPTION_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CMS_REMARKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CURRENT_SELL_RATE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CUSTOM_HOUSE_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESC_OF_GOODS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FREIGHT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.HBL_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.JOB_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MBL_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MEASUREMENT_BASIS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MEASUREMENT_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.OVERSEAS_SELL_AMOUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.OVERSEAS_TAX;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PLACE_OF_DELIVERY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PLACE_OF_RECEIPT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PORT_OF_DISCHARGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PORT_OF_DISCHARGE_COUNTRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PORT_OF_FINAL_DESTINATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PORT_OF_FINAL_DESTINATION_COUNTRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PORT_OF_LOADING;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PORT_OF_LOADING_COUNTRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.REVENUE_CALCULATED_VALUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.REVENUE_MEASUREMENT_BASIS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.REVENUE_MEASUREMENT_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.REVENUE_TOTAL_UNIT_COUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SELL_EXCHANGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TAXES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TAX_PERCENTAGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TENANT_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TOTAL_AMOUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TOTAL_TAX_AMOUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TOTAL_UNIT_COUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TRANSPORT_MODE;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.TaxPair;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentCANModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.BillChargesResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillingResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.fasterxml.jackson.core.type.TypeReference;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ShipmentCANReport extends IReport {

    public static final String REGEX_S_S_PATTERN = "%s %s";
    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private HblReport hblReport;

    @Autowired
    private IHblDao hblDao;

    public Boolean printWithoutTranslation;


    @Override
    public Map<String, Object> getData(Long id) throws RunnerException {
        ShipmentCANModel shipmentCANModel = (ShipmentCANModel) getDocumentModel(id);
        return populateDictionary(shipmentCANModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) throws RunnerException {
        ShipmentCANModel shipmentCANModel = new ShipmentCANModel();

        shipmentCANModel.shipmentDetails = getShipmentByQuery(id);
        shipmentCANModel.tenantDetails = getTenant();
        shipmentCANModel.consolidationModel = getFirstConsolidationFromShipmentId(id);
        shipmentCANModel.shipmentSettingsDetails = getShipmentSettings();
        shipmentCANModel.tenantSettingsResponse = getCurrentTenantSettings();
        shipmentCANModel.isHBL = getIsHbl(shipmentCANModel.shipmentDetails);
        return shipmentCANModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ShipmentCANModel shipmentCANModel = (ShipmentCANModel) documentModel;
        List<String> orgWithoutTranslation = new ArrayList<>();
        List<String> chargeTypesWithoutTranslation = new ArrayList<>();
        Map<String, Object> dictionary = hblReport.getData(shipmentCANModel.shipmentDetails.getId());
        populateShipmentOrganizationsLL(shipmentCANModel.shipmentDetails, dictionary, orgWithoutTranslation);
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
                List<BillChargesResponse> billChargesList = getBillChargesData(bill);
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
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS, AmountNumberFormatter.Format(shipmentCANModel.shipmentDetails.getFreightOverseas(), shipmentCANModel.shipmentDetails.getFreightOverseasCurrency(), shipmentCANModel.tenantSettingsResponse));
        if(shipmentCANModel.shipmentDetails != null && shipmentCANModel.shipmentDetails.getFreightOverseasCurrency() != null && !shipmentCANModel.shipmentDetails.getFreightOverseasCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS_CURRENCY, shipmentCANModel.shipmentDetails.getFreightOverseasCurrency());
        if(shipmentCANModel.shipmentDetails.getShipmentAddresses() != null && shipmentCANModel.shipmentDetails.getShipmentAddresses().size() > 0) {
            for (PartiesModel shipmentAddress: shipmentCANModel.shipmentDetails.getShipmentAddresses()) {
                if(shipmentAddress.getType().equals(CUSTOM_HOUSE_AGENT) && shipmentAddress.getOrgData() != null && getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME) != null) {
                    dictionary.put(CHAPartyDescription, getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME));
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
        TenantModel tenantModel = getTenant();
        dictionary.put(TENANT_NAME, tenantModel.tenantName);
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        dictionary.put(PLACE_OF_RECEIPT, shipmentCANModel.shipmentDetails.getCarrierDetails().getOrigin());
        dictionary.put(PLACE_OF_DELIVERY, shipmentCANModel.shipmentDetails.getCarrierDetails().getDestination());
        dictionary.put(TRANSPORT_MODE, shipmentCANModel.shipmentDetails.getTransportMode());
        dictionary.put(JOB_NO, shipmentCANModel.shipmentDetails.getShipmentId());
        dictionary.put(ReportConstants.CLIENT, getValueFromMap(shipmentCANModel.shipmentDetails.getClient().getOrgData(), "FullName"));
        dictionary.put(AIRLINE, shipmentCANModel.shipmentDetails.getCarrierDetails().getShippingLine());
        dictionary.put(CMS_REMARKS, shipmentCANModel.shipmentDetails.getGoodsDescription());
        if(shipmentCANModel.shipmentDetails.getVolumetricWeight() != null)
            dictionary.put(ReportConstants.V_WEIGHT_AND_UNIT, String.format(REGEX_S_S_PATTERN, ConvertToVolumetricWeightFormat(shipmentCANModel.shipmentDetails.getVolumetricWeight(), v1TenantSettingsResponse), shipmentCANModel.shipmentDetails.getVolumetricWeightUnit()));
        if(shipmentCANModel.shipmentDetails.getWeight() != null)
            dictionary.put(ReportConstants.WEIGHT_AND_UNIT, String.format(REGEX_S_S_PATTERN, ConvertToWeightNumberFormat(shipmentCANModel.shipmentDetails.getWeight(), v1TenantSettingsResponse), shipmentCANModel.shipmentDetails.getWeightUnit()));
        if(shipmentCANModel.shipmentDetails.getVolume() != null)
            dictionary.put(ReportConstants.VOLUME_AND_UNIT, String.format(REGEX_S_S_PATTERN, ConvertToVolumeNumberFormat(shipmentCANModel.shipmentDetails.getVolume(), v1TenantSettingsResponse), shipmentCANModel.shipmentDetails.getVolumeUnit()));
        dictionary.put(ReportConstants.MARKS_AND_NUMBER, shipmentCANModel.shipmentDetails.getMarksNum());
        dictionary.put(ReportConstants.NO_OF_PACKAGES, GetDPWWeightVolumeFormat(shipmentCANModel.shipmentDetails.getNoOfPacks() == null ? BigDecimal.ZERO : BigDecimal.valueOf(shipmentCANModel.shipmentDetails.getNoOfPacks()), 0, v1TenantSettingsResponse));
        if(shipmentCANModel.consolidationModel != null && shipmentCANModel.consolidationModel.getPayment() != null) {
            dictionary.put(FREIGHT, shipmentCANModel.consolidationModel.getPayment());
        }
        if(allBillCharges.size() > 0) {
            List<Map<String, Object>> billChargesDict = new ArrayList<>();
            for (BillChargesResponse billChargesResponse : allBillCharges) {
                billChargesDict.add(jsonHelper.convertValue(billChargesResponse, new TypeReference<>() {}));
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
//                        v.put(TOTAL_UNIT_COUNT, twoDecimalPlacesFormatDecimal(count));
                        v.put(TOTAL_UNIT_COUNT, GetDPWWeightVolumeFormat(count, 0, v1TenantSettingsResponse));
                    }
                }

                if(v.containsKey(SELL_EXCHANGE) && v.get(SELL_EXCHANGE) != null)
                    v.put(SELL_EXCHANGE, AmountNumberFormatter.FormatExchangeRate(new BigDecimal(v.get(SELL_EXCHANGE).toString()), shipmentCANModel.shipmentDetails.getFreightOverseasCurrency(), v1TenantSettingsResponse));
                if(v.containsKey(CURRENT_SELL_RATE) && v.get(CURRENT_SELL_RATE) != null)
                    v.put(CURRENT_SELL_RATE, AmountNumberFormatter.Format(new BigDecimal(v.get(CURRENT_SELL_RATE).toString()), shipmentCANModel.shipmentDetails.getFreightOverseasCurrency(), v1TenantSettingsResponse));
                if(v.containsKey(OVERSEAS_SELL_AMOUNT) && v.get(OVERSEAS_SELL_AMOUNT) != null)
                    v.put(OVERSEAS_SELL_AMOUNT, AmountNumberFormatter.Format(new BigDecimal(v.get(OVERSEAS_SELL_AMOUNT).toString()), shipmentCANModel.shipmentDetails.getFreightOverseasCurrency(), v1TenantSettingsResponse));
                if(v.containsKey(OVERSEAS_TAX) && v.get(OVERSEAS_TAX) != null)
                    v.put(OVERSEAS_TAX, AmountNumberFormatter.Format(new BigDecimal(v.get(OVERSEAS_TAX).toString()), shipmentCANModel.shipmentDetails.getFreightOverseasCurrency(), v1TenantSettingsResponse));
                if(v.containsKey(TAX_PERCENTAGE) && v.get(TAX_PERCENTAGE) != null)
                    v.put(TAX_PERCENTAGE, AmountNumberFormatter.Format(new BigDecimal(v.get(TAX_PERCENTAGE).toString()), shipmentCANModel.shipmentDetails.getFreightOverseasCurrency(), v1TenantSettingsResponse));
                if(v.containsKey(TOTAL_AMOUNT) && v.get(TOTAL_AMOUNT) != null)
                    v.put(TOTAL_AMOUNT, AmountNumberFormatter.Format(new BigDecimal(v.get(TOTAL_AMOUNT).toString()), shipmentCANModel.shipmentDetails.getFreightOverseasCurrency(), v1TenantSettingsResponse));
                if(v.containsKey(CHARGE_TYPE_CODE) && v.get(CHARGE_TYPE_CODE) != null) {
                    v.put(CHARGE_TYPE_DESCRIPTION_LL, GetChargeTypeDescriptionLL((String)v.get(CHARGE_TYPE_CODE), chargeTypesWithoutTranslation));
                }
            }
            dictionary.put(BILL_CHARGES, billChargesDict);
        }
        dictionary.put(ReportConstants.TOTAL_BILL_AMOUNT, AmountNumberFormatter.Format(BigDecimal.valueOf(totalBillAmount), shipmentCANModel.shipmentDetails.getFreightOverseasCurrency(), v1TenantSettingsResponse));
        dictionary.put(TAXES, taxes);
        dictionary.put(TOTAL_TAX_AMOUNT, AmountNumberFormatter.Format(BigDecimal.valueOf(totalTax), shipmentCANModel.shipmentDetails.getFreightOverseasCurrency(), v1TenantSettingsResponse));

        populateRaKcData(dictionary, shipmentCANModel.shipmentDetails);
        populateIGMInfo(shipmentCANModel.shipmentDetails, dictionary);
        HandleTranslationErrors(printWithoutTranslation, orgWithoutTranslation, chargeTypesWithoutTranslation);

        return dictionary;
    }
}
