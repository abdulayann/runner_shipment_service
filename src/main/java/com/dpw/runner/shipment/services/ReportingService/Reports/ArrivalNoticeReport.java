package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.ArrivalNoticeModel;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.response.BillChargesResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillingResponse;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getCityCountry;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;

@Component
@Slf4j
public class ArrivalNoticeReport extends IReport {

    @Autowired
    private JsonHelper jsonHelper;

    public Boolean printWithoutTranslation;

    @Override
    public Map<String, Object> getData(Long id) {
        ArrivalNoticeModel arrivalNoticeModel = (ArrivalNoticeModel) getDocumentModel(id);
        return populateDictionary(arrivalNoticeModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        ArrivalNoticeModel arrivalNoticeModel = new ArrivalNoticeModel();
        arrivalNoticeModel.shipmentDetails = getShipment(id);
        validateAirAndOceanDGCheck(arrivalNoticeModel.shipmentDetails);
        if(arrivalNoticeModel.shipmentDetails != null && arrivalNoticeModel.shipmentDetails.getConsolidationList() != null && !arrivalNoticeModel.shipmentDetails.getConsolidationList().isEmpty())
        {
            arrivalNoticeModel.consolidationDetails = arrivalNoticeModel.shipmentDetails.getConsolidationList().get(0);
        }
        arrivalNoticeModel.hbl = getHbl(id);
        arrivalNoticeModel.setContainers(new ArrayList<>());
        if(arrivalNoticeModel.shipmentDetails.getContainersList() != null)
        {
            for(ContainerModel container : arrivalNoticeModel.shipmentDetails.getContainersList())
                arrivalNoticeModel.getContainers().add(getShipmentContainer(container));
        }
        arrivalNoticeModel.usersDto = UserContext.getUser();
        return arrivalNoticeModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        ArrivalNoticeModel arrivalNoticeModel = (ArrivalNoticeModel) documentModel;
        List<String> orgWithoutTranslation = new ArrayList<>();
        List<String> chargeTypesWithoutTranslation = new ArrayList<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String json = jsonHelper.convertToJsonWithDateTimeFormatter(arrivalNoticeModel.shipmentDetails, getDPWDateFormatOrDefault(v1TenantSettingsResponse));
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        populateShipmentFields(arrivalNoticeModel.shipmentDetails, dictionary);
        populateUserFields(arrivalNoticeModel.usersDto, dictionary);
        populateBlFields(arrivalNoticeModel.hbl, dictionary);
        populateShipmentOrganizationsLL(arrivalNoticeModel.shipmentDetails, dictionary, orgWithoutTranslation);
        List<String> consignee = populateConsigneeData(dictionary, arrivalNoticeModel.shipmentDetails.getConsignee());
        dictionary.put(ReportConstants.CONSIGNEE,consignee);
        dictionary.put(ReportConstants.CONTAINER_COUNT_BY_CODE, getCountByContainerTypeCode(arrivalNoticeModel.getContainers()));
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, arrivalNoticeModel.getContainers());
        dictionary.put(ReportConstants.CURRENT_DATE, convertToDPWDateFormat(LocalDateTime.now(), v1TenantSettingsResponse.getDPWDateFormat(), v1TenantSettingsResponse));
        processShipmentContainers(arrivalNoticeModel, v1TenantSettingsResponse, dictionary);
        if(StringUtility.isNotEmpty(arrivalNoticeModel.shipmentDetails.getHouseBill())) {
            dictionary.put(ReportConstants.SHIPMENT_DETAILS_CARGOCONTROLNO, "80C2" + arrivalNoticeModel.shipmentDetails.getHouseBill());
        }
        getBillChargesDetails(arrivalNoticeModel, chargeTypesWithoutTranslation);
        processBillChargesDetails(arrivalNoticeModel, dictionary);

        Optional<ReferenceNumbersModel> referenceNumber = Optional.empty();

        if(arrivalNoticeModel.shipmentDetails.getAdditionalDetails() != null) {
            dictionary.put(NOTIFY_PARTY, ReportHelper.getOrgAddressDetails(arrivalNoticeModel.shipmentDetails.getAdditionalDetails().getNotifyParty()));
        }

        if (arrivalNoticeModel.shipmentDetails.getReferenceNumbersList() != null) {
            referenceNumber = arrivalNoticeModel.shipmentDetails.getReferenceNumbersList().stream().
                    filter(i -> i.getType().equals(ERN)).findFirst();
        }
        if (referenceNumber.isEmpty() && arrivalNoticeModel.consolidationDetails != null && arrivalNoticeModel.consolidationDetails.getReferenceNumbersList() != null) {
            referenceNumber = arrivalNoticeModel.consolidationDetails.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equals(ERN)).findFirst();
        }
        referenceNumber.ifPresent(i -> dictionary.put(EXPORT_REFERENCE_NUMBER, i.getReferenceNumber()));

        if(arrivalNoticeModel.consolidationDetails != null) {
            dictionary.put(AGENT_REFERENCE, arrivalNoticeModel.consolidationDetails.getAgentReference());
        }
        else if(arrivalNoticeModel.shipmentDetails.getAdditionalDetails() != null) {
            dictionary.put(AGENT_REFERENCE, arrivalNoticeModel.shipmentDetails.getAdditionalDetails().getAgentReference());
        }
        PartiesModel pickupFrom = null;
        if(arrivalNoticeModel.shipmentDetails.getPickupDetails() != null)
            pickupFrom = arrivalNoticeModel.shipmentDetails.getPickupDetails().getSourceDetail();
        if (pickupFrom != null && pickupFrom.getAddressData() != null)
        {
            Map<String, Object> addressMap = pickupFrom.getAddressData();
            populateAddress(addressMap, dictionary, ReportConstants.PICK_UP_FROM);
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put(ReportConstants.PICK_UP_FROM, address);
        }

        populateRaKcData(dictionary, arrivalNoticeModel.shipmentDetails);
        handleTranslationErrors(printWithoutTranslation, orgWithoutTranslation, chargeTypesWithoutTranslation);

        processShipmentPackingList(arrivalNoticeModel, dictionary);

        ReportHelper.addPartyNameAndAddressInCaps(arrivalNoticeModel.shipmentDetails.getConsigner(), dictionary, SHIPPER_NAME_IN_CAPS, SHIPPER_ADDRESS_IN_CAPS);
        ReportHelper.addPartyNameAndAddressInCaps(arrivalNoticeModel.shipmentDetails.getConsignee(), dictionary, CONSIGNEE_NAME_IN_CAPS, CONSIGNEE_ADDRESS_IN_CAPS);
        ReportHelper.addPartyNameAndAddressInCaps(arrivalNoticeModel.shipmentDetails.getAdditionalDetails().getImportBroker(), dictionary, DESTINATION_AGENT_NAME_IN_CAPS, DESTINATION_AGENT_ADDRESS_IN_CAPS);
        ReportHelper.addPartyNameAndAddressInCaps(arrivalNoticeModel.shipmentDetails.getAdditionalDetails().getExportBroker(), dictionary, ORIGIN_AGENT_NAME_IN_CAPS, ORIGIN_AGENT_ADDRESS_IN_CAPS);

        ReportHelper.addTenantDetails(dictionary, getTenant());

        if (arrivalNoticeModel.consolidationDetails != null) {
            this.populateConsolidationReportData(dictionary, null, arrivalNoticeModel.consolidationDetails.getId());
        }

        if (arrivalNoticeModel.shipmentDetails != null) {
            this.populateShipmentReportData(dictionary, null, arrivalNoticeModel.shipmentDetails.getId());
            this.getContainerDetails(arrivalNoticeModel.shipmentDetails, dictionary);
        }
        return dictionary;
    }

    private void processShipmentPackingList(ArrivalNoticeModel arrivalNoticeModel, Map<String, Object> dictionary) {
        if(!listIsNullOrEmpty(arrivalNoticeModel.shipmentDetails.getPackingList())) {
            getPackingDetails(arrivalNoticeModel.shipmentDetails, dictionary);
            dictionary.put(HAS_PACK_DETAILS, true);
            var hazardousCheck = arrivalNoticeModel.shipmentDetails.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getHazardous()) && x.getHazardous());
            var temperatureCheck = arrivalNoticeModel.shipmentDetails.getPackingList().stream().anyMatch(x -> !Objects.isNull(x.getIsTemperatureControlled()) && x.getIsTemperatureControlled());
            dictionary.put(HAS_DANGEROUS_GOODS, hazardousCheck);
            dictionary.put(HAS_TEMPERATURE_DETAILS, temperatureCheck);
        } else {
            dictionary.put(HAS_PACK_DETAILS, false);
        }
    }

    private void processShipmentContainers(ArrivalNoticeModel arrivalNoticeModel, V1TenantSettingsResponse v1TenantSettingsResponse, Map<String, Object> dictionary) {
        List<Map<String, Object>> valuesContainer = new ArrayList<>();
        for (ShipmentContainers shipmentContainers : arrivalNoticeModel.getContainers()) {
            valuesContainer.add(jsonHelper.convertValue(shipmentContainers, new TypeReference<>() {}));
        }
        for (Map<String, Object> v : valuesContainer) {
            if(v.containsKey(ReportConstants.GROSS_VOLUME) && v.get(ReportConstants.GROSS_VOLUME) != null)
                v.put(ReportConstants.GROSS_VOLUME, convertToVolumeNumberFormat(v.get(ReportConstants.GROSS_VOLUME), v1TenantSettingsResponse));
            if (v.containsKey(ReportConstants.GROSS_WEIGHT) && v.get(ReportConstants.GROSS_WEIGHT) != null)
                v.put(ReportConstants.GROSS_WEIGHT, convertToWeightNumberFormat(v.get(ReportConstants.GROSS_WEIGHT), v1TenantSettingsResponse));
            if (v.containsKey(ReportConstants.NET_WEIGHT) && v.get(ReportConstants.NET_WEIGHT) != null)
                v.put(ReportConstants.NET_WEIGHT, convertToWeightNumberFormat(new BigDecimal(v.get(ReportConstants.NET_WEIGHT).toString())));
        }
        dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, valuesContainer);
    }

    private void processBillChargesDetails(ArrivalNoticeModel arrivalNoticeModel, Map<String, Object> dictionary) {
        if(!Objects.isNull(arrivalNoticeModel.getArrivalNoticeBillCharges()) && !arrivalNoticeModel.getArrivalNoticeBillCharges().isEmpty()){
            var currency = arrivalNoticeModel.getArrivalNoticeBillCharges().stream().map(ArrivalNoticeModel.ArrivalNoticeBillCharges::getOverseasCurrency).filter(overseasCurrency ->!Objects.isNull(overseasCurrency)).findFirst().orElse("");
            BigDecimal sumOfTaxAmount = arrivalNoticeModel.getArrivalNoticeBillCharges().stream()
                    .map(ArrivalNoticeModel.ArrivalNoticeBillCharges::getTaxAmount).filter(Objects::nonNull)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            BigDecimal sumOfBillAmount = arrivalNoticeModel.getArrivalNoticeBillCharges().stream()
                    .map(ArrivalNoticeModel.ArrivalNoticeBillCharges::getBillAmount).filter(Objects::nonNull)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            dictionary.put(ReportConstants.SHIPMENT_BILLCHARGES_FREVENUEBILLCHARGES, arrivalNoticeModel.getArrivalNoticeBillCharges());
            dictionary.put(ReportConstants.SHIPMENT_BILLCHARGES_BILLCHARGESLOCALTAXSUMCOMMA, AmountNumberFormatter.format(sumOfTaxAmount, currency, getCurrentTenantSettings()));
            dictionary.put(ReportConstants.SHIPMENT_BILLCHARGES_BILLCHARGESSUM, AmountNumberFormatter.format(sumOfBillAmount, currency, getCurrentTenantSettings()));
            dictionary.put(ReportConstants.SHIPMENT_BILLCHARGES_OVERSEASCURRENCY, currency);
        }
    }

    private void getBillChargesDetails(ArrivalNoticeModel arrivalNoticeModel, List<String> chargeTypesWithoutTranslation){
        List<BillingResponse> billingsList = null;
        try {
            billingsList = getBillingData(arrivalNoticeModel.shipmentDetails.getGuid());
        }
        catch (Exception e) {
            log.error("Error fetching billing data for GUID: {}", arrivalNoticeModel.shipmentDetails.getGuid(), e);

        }
        List<BillChargesResponse> charges = new ArrayList<>();
        if(billingsList != null && !billingsList.isEmpty()) {
            for(BillingResponse billingResponse : billingsList) {
                List<BillChargesResponse> billChargesResponses = getBillChargesData(billingResponse);
                if(billChargesResponses != null) {
                    charges.addAll(billChargesResponses);
                }
            }
        }

        arrivalNoticeModel.setArrivalNoticeBillCharges(new ArrayList<>());
        if(!charges.isEmpty()) {
            for (var charge : charges){
                processArrivalNoticeCharge(arrivalNoticeModel, chargeTypesWithoutTranslation, charge);
            }
        }

    }

    private void processArrivalNoticeCharge(ArrivalNoticeModel arrivalNoticeModel, List<String> chargeTypesWithoutTranslation, BillChargesResponse charge) {
        var arrivalNoticeCharge = new ArrivalNoticeModel.ArrivalNoticeBillCharges();
        arrivalNoticeCharge.setChargeTypeDescription(charge.getChargeTypeDescription());
        arrivalNoticeCharge.setChargeTypeDescriptionLL(getChargeTypeDescriptionLL(charge.getChargeTypeCode(), chargeTypesWithoutTranslation));
        if(!Objects.isNull(charge.getOverseasSellAmount())){
            arrivalNoticeCharge.setSellAmount(AmountNumberFormatter.format(charge.getOverseasSellAmount(), charge.getLocalSellCurrency(), getCurrentTenantSettings()));
        }
        if(!Objects.isNull(charge.getLocalTax())){
            arrivalNoticeCharge.setTaxAmount(charge.getLocalTax());
        }
        if(!Objects.isNull(charge.getMeasurementBasis())){
            arrivalNoticeCharge.setMeasurementBasis(MeasurementBasis.getByValue(Integer.parseInt(charge.getMeasurementBasis())).getDescription());
        }
        if(!Objects.isNull(charge.getOverseasSellAmount())){
            arrivalNoticeCharge.setBillAmount(charge.getOverseasSellAmount());
        }
        if(!Objects.isNull(charge.getLocalCostCurrency())){
            arrivalNoticeCharge.setOverseasCurrency(charge.getLocalCostCurrency());
        }

        arrivalNoticeModel.getArrivalNoticeBillCharges().add(arrivalNoticeCharge);
    }
}
