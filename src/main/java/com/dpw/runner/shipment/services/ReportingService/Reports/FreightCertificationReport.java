package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHAPartyDescription;
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
import java.util.Objects;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FreightCertificationReport extends IReport {

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
        if (freightCertificationModel.shipmentDetails.getContainersList() != null && freightCertificationModel.shipmentDetails.getContainersList().size() > 0) {
            for (ContainerModel containers : freightCertificationModel.shipmentDetails.getContainersList()) {
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
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(freightCertificationModel.shipmentDetails, GetDPWDateFormatOrDefault(v1TenantSettingsResponse));
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        addTenantDetails(dictionary, freightCertificationModel.tenantDetails);

        List<String> consigner = null;
        if (freightCertificationModel.shipmentDetails.getConsigner() != null) {
            consigner = getOrgAddress(freightCertificationModel.shipmentDetails.getConsigner());
            if (freightCertificationModel.shipmentDetails.getConsigner().getOrgData() != null) {
                Map<String, Object> partyOrg = freightCertificationModel.shipmentDetails.getConsigner().getOrgData();
                if (!Boolean.TRUE.equals(freightCertificationModel.shipmentSettingsDetails.getDisableBlPartiesName()) && getValueFromMap(partyOrg, FULL_NAME1) != null) {
                    consigner.add(0, getValueFromMap(partyOrg, FULL_NAME1));
                }
            }
        }

        List<String> consignee = null;
        if (freightCertificationModel.shipmentDetails.getConsignee() != null) {
            consignee = getOrgAddress(freightCertificationModel.shipmentDetails.getConsignee());
            if (freightCertificationModel.shipmentDetails.getConsignee().getOrgData() != null) {
                Map<String, Object> partyOrg = freightCertificationModel.shipmentDetails.getConsignee().getOrgData();
                if (!Boolean.TRUE.equals(freightCertificationModel.shipmentSettingsDetails.getDisableBlPartiesName()) && getValueFromMap(partyOrg, FULL_NAME1) != null) {
                    consignee.add(0, getValueFromMap(partyOrg, FULL_NAME1));
                }
            }
        }

        List<String> notify = null;
        if (freightCertificationModel.shipmentDetails.getAdditionalDetails().getNotifyParty() != null) {
            notify = getOrgAddress(freightCertificationModel.shipmentDetails.getAdditionalDetails().getNotifyParty());
            if (freightCertificationModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData() != null) {
                Map<String, Object> partyOrg = freightCertificationModel.shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgData();
                if (!Boolean.TRUE.equals(freightCertificationModel.shipmentSettingsDetails.getDisableBlPartiesName()) && getValueFromMap(partyOrg, FULL_NAME1) != null) {
                    notify.add(0, getValueFromMap(partyOrg, FULL_NAME1));
                }
            }
        }
        V1TenantSettingsResponse tenantSettingsRow = getCurrentTenantSettings();

        List<String> tenantsDataList = getListOfStrings(freightCertificationModel.tenantDetails.tenantName, freightCertificationModel.tenantDetails.address1, freightCertificationModel.tenantDetails.address2,
                freightCertificationModel.tenantDetails.city, freightCertificationModel.tenantDetails.state, freightCertificationModel.tenantDetails.zipPostCode, freightCertificationModel.tenantDetails.country,
                freightCertificationModel.tenantDetails.email, freightCertificationModel.tenantDetails.websiteUrl, freightCertificationModel.tenantDetails.phone);
        if (tenantsDataList != null)
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
        dictionary.put(ReportConstants.CURRENT_DATE, ConvertToDPWDateFormat(LocalDateTime.now()));
        if (freightCertificationModel.shipmentDetails != null && freightCertificationModel.shipmentDetails.getFreightLocal() != null)
            dictionary.put(ReportConstants.FREIGHT_LOCAL, freightCertificationModel.shipmentDetails.getFreightLocal());
        if (freightCertificationModel.shipmentDetails != null && freightCertificationModel.shipmentDetails.getFreightLocalCurrency() != null && !freightCertificationModel.shipmentDetails.getFreightLocalCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_LOCAL_CURRENCY, freightCertificationModel.shipmentDetails.getFreightLocalCurrency());
        if (freightCertificationModel.shipmentDetails != null && freightCertificationModel.shipmentDetails.getFreightOverseas() != null)
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS, AmountNumberFormatter.Format(freightCertificationModel.shipmentDetails.getFreightOverseas(), freightCertificationModel.shipmentDetails.getFreightOverseasCurrency(), tenantSettingsRow));
        if (freightCertificationModel.shipmentDetails != null && freightCertificationModel.shipmentDetails.getFreightOverseasCurrency() != null && !freightCertificationModel.shipmentDetails.getFreightOverseasCurrency().isEmpty())
            dictionary.put(ReportConstants.FREIGHT_OVERSEAS_CURRENCY, freightCertificationModel.shipmentDetails.getFreightOverseasCurrency());
        if (freightCertificationModel.shipmentDetails.getShipmentAddresses() != null && freightCertificationModel.shipmentDetails.getShipmentAddresses().size() > 0) {
            for (PartiesModel shipmentAddress : freightCertificationModel.shipmentDetails.getShipmentAddresses()) {
                if (shipmentAddress.getType().equals(CUSTOM_HOUSE_AGENT) && shipmentAddress.getOrgData() != null
                        && getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME) != null) {
                    dictionary.put(CHAPartyDescription, getValueFromMap(shipmentAddress.getOrgData(), FULL_NAME));
                }
            }
        }
        populateIGMInfo(freightCertificationModel.shipmentDetails, dictionary);

        List<BillingResponse> billingsList = getBillingData(freightCertificationModel.shipmentDetails.getGuid());

        // Fetch the last posted invoice date
        // Since there is only 1 shipment fetch lastDate once.
        LocalDateTime lastDate = Boolean.TRUE.equals(billingServiceUrlConfig.getEnableBillingIntegration()) ?
                billingServiceAdapter.fetchLastPostedInvoiceDate(LastPostedInvoiceDateRequest.builder()
                        .moduleGuid(freightCertificationModel.getShipmentDetails().getGuid().toString())
                        .moduleType(Constants.SHIPMENT).build())
                : LocalDateTime.MIN;

        double totalAmount = 0;
        String currency = null;
        if (billingsList != null && billingsList.size() > 0) {
            for (BillingResponse bill : billingsList) {
                if (Boolean.FALSE.equals(billingServiceUrlConfig.getEnableBillingIntegration())) {
                    List<ArObjectResponse> arObjectsList = getArObjectData(bill.getGuid());
                    if (arObjectsList != null && arObjectsList.size() > 0) {
                        for (ArObjectResponse arObject : arObjectsList) {
                            if (arObject.getInvoiceDate() != null && arObject.getInvoiceDate().isAfter(lastDate)) {
                                lastDate = arObject.getInvoiceDate();
                            }
                        }
                    }
                }
                List<BillChargesResponse> billChargesList = getBillChargesData(bill);
                boolean currencyFlag = false;
                if (billChargesList != null && billChargesList.size() > 0) {
                    for (BillChargesResponse billCharge : billChargesList) {
                        ChargeTypesResponse chargeTypesResponse = getChargeTypesData(billCharge);
                        if (chargeTypesResponse != null && Objects.equals(chargeTypesResponse.getServices(), "Freight")) {
                            if (billCharge.getOverseasSellAmount() != null) {
                                if (currency == null) {
                                    currency = billCharge.getOverseasSellCurrency();
                                    totalAmount = totalAmount + billCharge.getOverseasSellAmount().doubleValue();
                                } else if (!billCharge.getOverseasSellCurrency().equals(currency)) {
                                    currencyFlag = true;
                                    break;
                                } else
                                    totalAmount = totalAmount + billCharge.getOverseasSellAmount().doubleValue();
                            }
                        }
                    }
                    if (currencyFlag) {
                        totalAmount = 0;
                        currency = null;
                        for (BillChargesResponse billCharge : billChargesList) {
                            ChargeTypesResponse chargeTypesResponse = getChargeTypesData(billCharge);
                            if (chargeTypesResponse != null && chargeTypesResponse.getServices().equals("Freight")) {
                                if (billCharge.getLocalSellAmount() != null) {
                                    if (currency == null) {
                                        currency = billCharge.getLocalSellCurrency();
                                    }
                                    totalAmount = totalAmount + billCharge.getLocalSellAmount().doubleValue();
                                }
                            }
                        }
                    }
                }
            }
        }
        DecimalFormat decimalFormat = new DecimalFormat("0.00");
        if (!lastDate.equals(LocalDateTime.MIN))
            dictionary.put(INVOICE_DATE, lastDate.format(DateTimeFormatter.ofPattern("dd MMM yyyy")));
        else
            dictionary.put(INVOICE_DATE, null);
        if (totalAmount != 0) {
            String strTotalAmount = decimalFormat.format(totalAmount);
            dictionary.put(TOTAL_AMOUNT, strTotalAmount);
            dictionary.put(TOTAL_AMOUNT_CURRENCY, currency);
        } else {
            dictionary.put(TOTAL_AMOUNT, null);
            dictionary.put(TOTAL_AMOUNT_CURRENCY, null);
        }
        return dictionary;
    }
}
