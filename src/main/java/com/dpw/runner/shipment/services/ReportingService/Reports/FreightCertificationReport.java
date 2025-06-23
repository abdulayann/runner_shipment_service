package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHA_PARTY_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CUSTOM_HOUSE_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME1;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.INVOICE_DATE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TOTAL_AMOUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TOTAL_AMOUNT_CURRENCY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.addTenantDetails;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getListOfStrings;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.numberToWords;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.FreightCertificationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IBillingServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.request.billing.LastPostedInvoiceDateRequest;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.ArObjectResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillChargesResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillingResponse;
import com.dpw.runner.shipment.services.masterdata.response.ChargeTypesResponse;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FreightCertificationReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private CommonUtils commonUtils;

    @Autowired
    private IBillingServiceAdapter billingServiceAdapter;

    @Autowired
    private BillingServiceUrlConfig billingServiceUrlConfig;

    @Override
    public Map<String, Object> getData(Long id) {
        FreightCertificationModel freightCertificationModel = (FreightCertificationModel) getDocumentModel(id);
        return populateDictionary(freightCertificationModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        FreightCertificationModel freightCertificationModel = new FreightCertificationModel();
        freightCertificationModel.shipmentDetails = getShipment(id);
        validateAirAndOceanDGCheck(freightCertificationModel.shipmentDetails);
        freightCertificationModel.tenantDetails = getTenant();
        freightCertificationModel.setAllContainersList(new ArrayList<>());
        if(freightCertificationModel.shipmentDetails.getContainersList() != null && !freightCertificationModel.shipmentDetails.getContainersList().isEmpty()) {
            for (ContainerModel containers: freightCertificationModel.shipmentDetails.getContainersList()) {
                ShipmentContainers shipmentContainers = jsonHelper.convertValue(containers, ShipmentContainers.class);
                freightCertificationModel.getAllContainersList().add(shipmentContainers);
            }
        }
        freightCertificationModel.noofpackages_word = numberToWords(freightCertificationModel.shipmentDetails.getNoOfPacks());
        freightCertificationModel.userdisplayname = UserContext.getUser().DisplayName;
        freightCertificationModel.shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        return freightCertificationModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        FreightCertificationModel freightCertificationModel = (FreightCertificationModel) documentModel;
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(freightCertificationModel.shipmentDetails, getDPWDateFormatOrDefault(v1TenantSettingsResponse));
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        addTenantDetails(dictionary, freightCertificationModel.tenantDetails);

        List<String> consigner = getPartyDetails(freightCertificationModel.shipmentDetails.getConsigner(), freightCertificationModel);

        List<String> consignee = getPartyDetails(freightCertificationModel.shipmentDetails.getConsignee(), freightCertificationModel);

        List<String> notify = getPartyDetails(freightCertificationModel.shipmentDetails.getAdditionalDetails().getNotifyParty(), freightCertificationModel);
        V1TenantSettingsResponse tenantSettingsRow = getCurrentTenantSettings();

        List<String> tenantsDataList = getListOfStrings(freightCertificationModel.tenantDetails.tenantName, freightCertificationModel.tenantDetails.address1, freightCertificationModel.tenantDetails.address2,
                freightCertificationModel.tenantDetails.city, freightCertificationModel.tenantDetails.state, freightCertificationModel.tenantDetails.zipPostCode, freightCertificationModel.tenantDetails.country,
                freightCertificationModel.tenantDetails.email, freightCertificationModel.tenantDetails.websiteUrl, freightCertificationModel.tenantDetails.phone);
        dictionary.put(ReportConstants.TENANT, tenantsDataList);
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(freightCertificationModel.getAllContainersList()));
        dictionary.put(ReportConstants.CLIENT_NAME, getValueFromMap(freightCertificationModel.shipmentDetails.getClient().getOrgData(), FULL_NAME1));
        dictionary.put(ReportConstants.CONSIGNER, consigner);
        dictionary.put(ReportConstants.CONSIGNEE, consignee);
        dictionary.put(ReportConstants.NOTIFY_PARTY, notify);
        dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, notify);
        dictionary.put(ReportConstants.CONSIGNEE_FREETEXT, consignee);
        dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
        dictionary.put(ReportConstants.TENANT_CITY, freightCertificationModel.tenantDetails.city);
        dictionary.put(ReportConstants.TENANT_COUNTRY, freightCertificationModel.tenantDetails.country);
        dictionary.put(ReportConstants.NO_OF_PACKAGES_WORD, freightCertificationModel.noofpackages_word);
        dictionary.put(ReportConstants.USER_DISPLAY_NAME, freightCertificationModel.userdisplayname);
        dictionary.put(ReportConstants.CURRENT_DATE, convertToDPWDateFormat(LocalDateTime.now()));
        if(freightCertificationModel.shipmentDetails != null && freightCertificationModel.shipmentDetails.getFreightLocal() != null)
            dictionary.put(ReportConstants.FREIGHT_LOCAL, freightCertificationModel.shipmentDetails.getFreightLocal());
        if(freightCertificationModel.shipmentDetails != null && freightCertificationModel.shipmentDetails.getFreightLocalCurrency() != null && !freightCertificationModel.shipmentDetails.getFreightLocalCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_LOCAL_CURRENCY, freightCertificationModel.shipmentDetails.getFreightLocalCurrency());
        if(freightCertificationModel.shipmentDetails != null && freightCertificationModel.shipmentDetails.getFreightOverseas() != null)
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS, AmountNumberFormatter.format(freightCertificationModel.shipmentDetails.getFreightOverseas(), freightCertificationModel.shipmentDetails.getFreightOverseasCurrency(), tenantSettingsRow));
        if(freightCertificationModel.shipmentDetails != null && freightCertificationModel.shipmentDetails.getFreightOverseasCurrency() != null && !freightCertificationModel.shipmentDetails.getFreightOverseasCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS_CURRENCY, freightCertificationModel.shipmentDetails.getFreightOverseasCurrency());
        processShipmentAddress(freightCertificationModel, dictionary);
        populateIGMInfo(freightCertificationModel.shipmentDetails, dictionary);

        processBillingList(freightCertificationModel, dictionary);

        if (freightCertificationModel.shipmentDetails != null) {
            if (ObjectUtils.isNotEmpty(freightCertificationModel.shipmentDetails.getConsolidationList())) {
                ConsolidationModel consolidationModel = freightCertificationModel.shipmentDetails.getConsolidationList().get(0);
                this.populateConsolidationReportData(dictionary, null, consolidationModel.getId());
            }
        }

        return dictionary;
    }

    private List<String> getPartyDetails(PartiesModel shipmentDetails, FreightCertificationModel freightCertificationModel) {
        List<String> consigner = null;
        if (shipmentDetails != null) {
            consigner = getOrgAddress(shipmentDetails);
            if (shipmentDetails.getOrgData() != null) {
                Map<String, Object> partyOrg = shipmentDetails.getOrgData();
                if (!Boolean.TRUE.equals(freightCertificationModel.shipmentSettingsDetails.getDisableBlPartiesName()) && getValueFromMap(partyOrg, FULL_NAME1) != null) {
                    consigner.add(0, getValueFromMap(partyOrg, FULL_NAME1));
                }
            }
        }
        return consigner;
    }

    private void processShipmentAddress(FreightCertificationModel freightCertificationModel, Map<String, Object> dictionary) {
        if (freightCertificationModel.shipmentDetails.getShipmentAddresses() != null && !freightCertificationModel.shipmentDetails.getShipmentAddresses().isEmpty()) {
            for (PartiesModel shipmentAddress : freightCertificationModel.shipmentDetails.getShipmentAddresses()) {
                if (shipmentAddress.getType().equals(CUSTOM_HOUSE_AGENT) && shipmentAddress.getOrgData() != null
                        && getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME) != null) {
                    dictionary.put(CHA_PARTY_DESCRIPTION, getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME));
                }
            }
        }
    }

    private void processBillingList(FreightCertificationModel freightCertificationModel, Map<String, Object> dictionary) {
        List<BillingResponse> billingsList = getBillingData(freightCertificationModel.shipmentDetails.getGuid());
        LocalDateTime lastDate = fetchLastPostedInvoiceDate(freightCertificationModel);
        BillingSummary billingSummary = calculateBillingSummary(billingsList, lastDate);
        updateDictionary(dictionary, billingSummary);
    }

    private LocalDateTime fetchLastPostedInvoiceDate(FreightCertificationModel freightCertificationModel) {
        if (Boolean.TRUE.equals(billingServiceUrlConfig.getEnableBillingIntegration())) {
            return billingServiceAdapter.fetchLastPostedInvoiceDate(LastPostedInvoiceDateRequest.builder()
                    .moduleGuid(freightCertificationModel.getShipmentDetails().getGuid().toString())
                    .moduleType(Constants.SHIPMENT).build());
        }
        return LocalDateTime.MIN;
    }

    private BillingSummary calculateBillingSummary(List<BillingResponse> billingsList, LocalDateTime lastDate) {
        BillingSummary summary = new BillingSummary();
        if (billingsList == null || billingsList.isEmpty()) {
            return summary;
        }

        for (BillingResponse bill : billingsList) {
            if (Boolean.FALSE.equals(billingServiceUrlConfig.getEnableBillingIntegration())) {
                lastDate = updateLastDateFromArObjects(bill, lastDate);
            }
            processBillCharges(bill, summary);
        }
        summary.setLastDate(lastDate);
        return summary;
    }

    private LocalDateTime updateLastDateFromArObjects(BillingResponse bill, LocalDateTime lastDate) {
        List<ArObjectResponse> arObjectsList = getArObjectData(bill.getGuid());
        if (arObjectsList != null && !arObjectsList.isEmpty()) {
            for (ArObjectResponse arObject : arObjectsList) {
                if (arObject.getInvoiceDate() != null && arObject.getInvoiceDate().isAfter(lastDate)) {
                    lastDate = arObject.getInvoiceDate();
                }
            }
        }
        return lastDate;
    }

    private void processBillCharges(BillingResponse bill, BillingSummary summary) {
        List<BillChargesResponse> billChargesList = getBillChargesData(bill);
        if (billChargesList == null || billChargesList.isEmpty()) {
            return;
        }

        boolean currencyFlag = false;
        for (BillChargesResponse billCharge : billChargesList) {
            ChargeTypesResponse chargeTypesResponse = getChargeTypesData(billCharge);
            if (chargeTypesResponse != null && "Freight".equals(chargeTypesResponse.getServices())) {
                currencyFlag = processCharge(billCharge, summary, currencyFlag);
            }
        }

        if (currencyFlag) {
            processLocalCharges(billChargesList, summary);
        }
    }

    private boolean processCharge(BillChargesResponse billCharge, BillingSummary summary, boolean currencyFlag) {
        if (billCharge.getOverseasSellAmount() != null) {
            if (summary.getCurrency() == null) {
                summary.setCurrency(billCharge.getOverseasSellCurrency());
                summary.addToTotalAmount(billCharge.getOverseasSellAmount().doubleValue());
            } else if (!billCharge.getOverseasSellCurrency().equals(summary.getCurrency())) {
                return true;
            } else {
                summary.addToTotalAmount(billCharge.getOverseasSellAmount().doubleValue());
            }
        }
        return currencyFlag;
    }

    private void processLocalCharges(List<BillChargesResponse> billChargesList, BillingSummary summary) {
        summary.resetAmountAndCurrency();
        for (BillChargesResponse billCharge : billChargesList) {
            ChargeTypesResponse chargeTypesResponse = getChargeTypesData(billCharge);
            if (chargeTypesResponse != null && "Freight".equals(chargeTypesResponse.getServices()) && billCharge.getLocalSellAmount() != null) {
                if (summary.getCurrency() == null) {
                    summary.setCurrency(billCharge.getLocalSellCurrency());
                }
                summary.addToTotalAmount(billCharge.getLocalSellAmount().doubleValue());
            }
        }
    }

    private void updateDictionary(Map<String, Object> dictionary, BillingSummary summary) {
        DecimalFormat decimalFormat = new DecimalFormat("0.00");
        if (!summary.getLastDate().equals(LocalDateTime.MIN)) {
            dictionary.put(INVOICE_DATE, summary.getLastDate().format(DateTimeFormatter.ofPattern("dd MMM yyyy")));
        } else {
            dictionary.put(INVOICE_DATE, null);
        }
        if (summary.getTotalAmount() != 0) {
            dictionary.put(TOTAL_AMOUNT, decimalFormat.format(summary.getTotalAmount()));
            dictionary.put(TOTAL_AMOUNT_CURRENCY, summary.getCurrency());
        } else {
            dictionary.put(TOTAL_AMOUNT, null);
            dictionary.put(TOTAL_AMOUNT_CURRENCY, null);
        }
    }

    @Getter
    static class BillingSummary {
        private double totalAmount = 0;
        @Setter
        private String currency = null;
        @Setter
        private LocalDateTime lastDate;

        public void addToTotalAmount(double amount) {
            this.totalAmount += amount;
        }

        public void resetAmountAndCurrency() {
            this.totalAmount = 0;
            this.currency = null;
        }
    }
}
