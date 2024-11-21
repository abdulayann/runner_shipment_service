package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.OtherChargesResponse;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.*;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.dto.request.reportService.CompanyDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.enums.ChargesDue;
import com.dpw.runner.shipment.services.entity.enums.RateClass;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.base.Strings;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.emptyIfNull;

@Component
@Slf4j
public class HawbReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IV1Service v1Service;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public Map<String, Object> getData(Long id) {
        HawbModel hawbModel = (HawbModel) getDocumentModel(id);
        return populateDictionary(hawbModel);
    }

    @Override
    public IDocumentModel getDocumentModel(Long id) {
        HawbModel hawbModel = new HawbModel();
        hawbModel.usersDto = UserContext.getUser();
        hawbModel.shipmentDetails = getShipment(id);
        validateAirAndOceanDGCheck(hawbModel.shipmentDetails);
        validateAirDGCheckShipments(hawbModel.shipmentDetails);
        if(hawbModel.shipmentDetails != null && hawbModel.shipmentDetails.getConsolidationList() != null && !hawbModel.shipmentDetails.getConsolidationList().isEmpty())
        {
            hawbModel.setConsolidationDetails(hawbModel.shipmentDetails.getConsolidationList().get(0));
            hawbModel.setMawb(getMawb(hawbModel.getConsolidationDetails().getId(), false));
        }
        hawbModel.awb = getHawb(id);
        hawbModel.setEntityType("HAWB");
        return hawbModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {

        HawbModel hawbModel = (HawbModel) documentModel;
        String json;
        CarrierDetailModel carrierDetailModel;
        if(hawbModel.shipmentDetails != null ) {
            json = jsonHelper.convertToJsonWithDateTimeFormatter(hawbModel.shipmentDetails, GetDPWDateFormatOrDefault());
            carrierDetailModel = hawbModel.getShipmentDetails().getCarrierDetails();
        } else {
            json = jsonHelper.convertToJsonWithDateTimeFormatter(hawbModel.getConsolidationDetails(), GetDPWDateFormatOrDefault());
            carrierDetailModel = hawbModel.getConsolidationDetails().getCarrierDetails();
        }
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();

        //TODO- Tenant data
//        var tenantDetails = ReportHelper.getOrgAddress(siData.tenant.TenantName, siData.tenant.Address1, siData.tenant.Address2, siData.tenant.City, siData.tenant.Email, siData.tenant.Phone, siData.tenant.ZipPostCode, siData.tenant.State);
//        dictionary[ReportConstants.AGENT] = tenantDetails;

        populateUserFields(hawbModel.usersDto, dictionary);

        // Get the shipmentInforRow
        AwbShipmentInfo shipmentInfo = hawbModel.awb.getAwbShipmentInfo();

        if(shipmentInfo != null){
            List<String> awbShipper = getAwbFormattedDetails(shipmentInfo.getShipperName(), shipmentInfo.getShipperAddress(), shipmentInfo.getShipperCity(), shipmentInfo.getShipperState(), shipmentInfo.getShipperZipCode(), shipmentInfo.getShipperCountry(), shipmentInfo.getShipperContactName(), shipmentInfo.getShipperPhone(), shipmentInfo.getShipperTaxRegistrationNumber());
            List<String> awbConsignee = getAwbFormattedDetails(shipmentInfo.getConsigneeName(), shipmentInfo.getConsigneeAddress(), shipmentInfo.getConsigneeCity(), shipmentInfo.getConsigneeState(), shipmentInfo.getConsigneeZipCode(), shipmentInfo.getConsigneeCountry(), shipmentInfo.getConsigneeContactName(), shipmentInfo.getConsigneePhone(), shipmentInfo.getConsigneeTaxRegistrationNumber());
            List<String> shipper = getFormattedDetails(shipmentInfo.getShipperName(), shipmentInfo.getShipperAddress(), shipmentInfo.getShipperCountry(), shipmentInfo.getShipperState(), shipmentInfo.getShipperCity(), shipmentInfo.getShipperZipCode(), shipmentInfo.getShipperPhone());
            List<String> consignee = getFormattedDetails(shipmentInfo.getConsigneeName(), shipmentInfo.getConsigneeAddress(), shipmentInfo.getConsigneeCountry(), shipmentInfo.getConsigneeState(), shipmentInfo.getConsigneeCity(), shipmentInfo.getConsigneeZipCode(), shipmentInfo.getConsigneePhone());
            dictionary.put(ReportConstants.AWB_SHIPPER_ADDRESS, awbShipper);
            dictionary.put(ReportConstants.AWB_CONSIGNEE_ADDRESS, awbConsignee);
            dictionary.put(ReportConstants.SHIPPER_ADDRESS, shipper);
            dictionary.put(ReportConstants.CONSIGNEE_ADDRESS,  consignee);
            dictionary.put(ReportConstants.ISSUING_CARRIER_AGENT_NAME, StringUtility.toUpperCase(shipmentInfo.getIssuingAgentName()));
            dictionary.put(ReportConstants.ISSUiNG_CARRIER_CITY, cityFromOrganizations(shipmentInfo.getIssuingAgentName()).toUpperCase());
            dictionary.put(ReportConstants.AGENT_IATA_CODE , upperCase(shipmentInfo.getIataCode()));
            dictionary.put(ReportConstants.CASSCODE , upperCase(shipmentInfo.getAgentCASSCode()));
            dictionary.put(ReportConstants.FIRST_CARRIER, shipmentInfo.getFirstCarrier());
            dictionary.put(ReportConstants.SHIPPER_ACCOUNT_NUMBER, shipmentInfo.getShipperAccountNumber());
            dictionary.put(ReportConstants.CONSIGNEE_ACCOUNT_NUMBER, shipmentInfo.getConsigneeAccountNumber());
            dictionary.put(ReportConstants.ACCOUNT_NUMBER, shipmentInfo.getAccountNumber());

            Set<String> locCodes = new HashSet<>();

            if (shipmentInfo.getOriginAirport() != null)
            {
                locCodes.add(shipmentInfo.getOriginAirport());
            }

            if (shipmentInfo.getDestinationAirport() != null)
            {
                locCodes.add(shipmentInfo.getDestinationAirport());
            }

            Map<String, UnlocationsResponse> locCodeMap = getLocationData(locCodes, EntityTransferConstants.NAME);
            UnlocationsResponse  originAirport = locCodeMap.get(shipmentInfo.getOriginAirport());
            UnlocationsResponse  destinationAirport = locCodeMap.get(shipmentInfo.getDestinationAirport());

            Set<String> masterDataQuery = new HashSet<>();
            String AwbNumber = "";
            EntityTransferMasterLists paymentTerms = null;
            ConsolidationModel consolRow = hawbModel.getConsolidationDetails();

            if (hawbModel.getEntityType().equalsIgnoreCase(AwbConstants.MAWB))
            {
                dictionary.put(ReportConstants.JOB_NUMBER , consolRow.getConsolidationNumber());
                dictionary.put(ReportConstants.MAWB_NO , shipmentInfo.getAwbNumber());
                dictionary.put(ReportConstants.NEUTRAL_AWB_NO, shipmentInfo.getAwbNumber());
                AwbNumber = shipmentInfo.getAwbNumber();

                if (StringUtility.isNotEmpty(consolRow.getPayment()))
                {
                    masterDataQuery.add(MasterDataType.PAYMENT.getDescription() + "#" + consolRow.getPayment());
                }
                dictionary.put(ReportConstants.IS_DMAWB, false);
                dictionary.put(ReportConstants.IS_B2BMAWB, true);
                //dictionary["PrintUserName"] = consolRow.InsertUserIdUsername;
            }
            else
            {
                ShipmentModel shipmentRow = hawbModel.shipmentDetails;
                dictionary.put(ReportConstants.TRANSPORT_MODE,  shipmentRow.getTransportMode());
                dictionary.put(ReportConstants.JOB_NUMBER, shipmentRow.getShipmentId());
                if(hawbModel.getEntityType().equalsIgnoreCase(AwbConstants.DMAWB))
                {
                    dictionary.put(ReportConstants.IS_DMAWB, true);
                    dictionary.put(ReportConstants.IS_B2BMAWB, false);
                    dictionary.put(ReportConstants.MAWB_NO, shipmentInfo.getAwbNumber());
                    AwbNumber = shipmentInfo.getAwbNumber();
                }
                else {
                    dictionary.put(ReportConstants.HAWB_NO, shipmentInfo.getAwbNumber());
                    // Also show MAWB number when printing HAWB
                    dictionary.put(ReportConstants.MAWB_NO, hawbModel.getShipmentDetails().getMasterBill());
                    AwbNumber = hawbModel.getMawb() == null || hawbModel.getMawb().getAwbShipmentInfo() == null || StringUtility.isEmpty(hawbModel.getMawb().getAwbShipmentInfo().getAwbNumber()) ? StringUtility.convertToString(hawbModel.getShipmentDetails().getMasterBill()) : hawbModel.getMawb().getAwbShipmentInfo().getAwbNumber();
                }
                dictionary.put(ReportConstants.NEUTRAL_AWB_NO, shipmentInfo.getAwbNumber());

                if (StringUtility.isNotEmpty(shipmentRow.getPaymentTerms())) {
                    masterDataQuery.add(MasterDataType.PAYMENT.getDescription() + "#" + shipmentRow.getPaymentTerms());
                }
                dictionary.put(ReportConstants.PRINT_USER_NAME, shipmentRow.getAssignedTo());
                if (shipmentRow != null && shipmentRow.getDeliveryDetails() != null)
                    dictionary.put(ReportConstants.SHIPMENT_DELIVERY_DELIVERYINSTRUCTION, shipmentRow.getDeliveryDetails().getPickupDeliveryInstruction());

                if (shipmentRow != null && shipmentRow.getPickupDetails() != null)
                    dictionary.put(ReportConstants.SHIPMENT_PICKUP_PICKUPINSTRUCTION, shipmentRow.getPickupDetails().getPickupDeliveryInstruction());

                if(shipmentRow.getReferenceNumbersList() != null) {
                    List<String> exporterReferenceNumberList = new ArrayList<>();
                    List<String> customsReferenceNumberList = new ArrayList<>();
                    List<String> forwarderReferenceNumberList = new ArrayList<>();
                    for(var referenceNumber : shipmentRow.getReferenceNumbersList()) {
                        switch (referenceNumber.getType()) {
                            case ERN -> exporterReferenceNumberList.add(referenceNumber.getReferenceNumber());
                            case CEN -> customsReferenceNumberList.add(referenceNumber.getReferenceNumber());
                            case FRN -> forwarderReferenceNumberList.add(referenceNumber.getReferenceNumber());
                            default -> {}
                        }
                    }

                    if(!exporterReferenceNumberList.isEmpty())
                        dictionary.put(EXPORTER_REFERENCE_NUMBER, String.join(",", exporterReferenceNumberList));
                    if(!customsReferenceNumberList.isEmpty())
                        dictionary.put(CUSTOMS_REFERENCE_NUMBER, String.join(",", customsReferenceNumberList));
                    if(!forwarderReferenceNumberList.isEmpty())
                        dictionary.put(FORWARDER_REFERENCE_NUMBER, String.join(",", forwarderReferenceNumberList));
                }
                if(!Strings.isNullOrEmpty(shipmentRow.getCarrierDetails().getShippingLine())){
                    CarrierMasterData carrierData = getCarrier(shipmentRow.getCarrierDetails().getShippingLine());
                    if(!Objects.isNull(carrierData))
                        dictionary.put(CARRIER_NAME, carrierData.getItemDescription());
                }
                PickupDeliveryDetailsModel pickup = shipmentRow.getPickupDetails();
                if(pickup != null && pickup.getTransporterDetail() != null){
                    dictionary.put(PRE_CARRIAGE_PARTY, pickup.getTransporterDetail().getOrgData() != null ?
                            pickup.getTransporterDetail().getOrgData().get("FullName") : "");
                }
                if(!Objects.isNull(shipmentRow.getPackingList()) && !shipmentRow.getPackingList().isEmpty()){
                    var values = shipmentRow.getPackingList().stream()
                        .map(i -> jsonHelper.convertJsonToMap(jsonHelper.convertToJson(i)))
                        .toList();
                    values.forEach(v -> {
                        if(v.containsKey(COMMODITY_GROUP) && v.get(COMMODITY_GROUP) != null){
                            MasterData commodity = getMasterListData(MasterDataType.COMMODITY_GROUP, v.get(COMMODITY_GROUP).toString());
                            if(!Objects.isNull(commodity))
                                v.put(PACKS_COMMODITY_GROUP, commodity.getItemDescription());
                        }
                    });
                    dictionary.put(SHIPMENT_PACKS, values);
                }
            }
            if(StringUtility.isNotEmpty(AwbNumber)){
                AwbNumber = AwbNumber.replace("-", "");
                dictionary.put(ReportConstants.MAWB_NO3 , AwbNumber.substring(0, Math.min(3, AwbNumber.length())));
                if(AwbNumber.length() > 3) dictionary.put(ReportConstants.MAWB_REMAINING, AwbNumber.substring(3));
            }
            var shipInfo = hawbModel.getAwb().getAwbShipmentInfo();
            dictionary.put(ISSUING_AGENT_ADDRESS, constructAddressForAwb(shipInfo.getIssuingAgentAddress(), shipInfo.getIssuingAgentCountry(), shipInfo.getIssuingAgentState(), shipInfo.getIssuingAgentCity(), shipInfo.getIssuingAgentZipCode(), shipInfo.getIssuingAgentPhone()));
            dictionary.put(AWB_ISSUING_AGENT_ADDRESS, getAwbFormattedDetails(shipInfo.getIssuingAgentName(),shipInfo.getIssuingAgentAddress(), shipInfo.getIssuingAgentCity(), shipInfo.getIssuingAgentState(), shipInfo.getIssuingAgentZipCode(), shipInfo.getIssuingAgentCountry(), shipInfo.getIssuingAgentContactName(), shipInfo.getIssuingAgentPhone(), shipInfo.getIssuingAgentTaxRegistrationNumber()));
            AwbCargoInfo cargoInfoRows = hawbModel.getAwb().getAwbCargoInfo();
            String NtrQtyGoods = null;
            EntityTransferMasterLists paymentCodeDetails = null;
            if(cargoInfoRows != null){
                dictionary.put(ReportConstants.REFERENCE_NUMBER, cargoInfoRows.getReferenceNumber());
                dictionary.put(ReportConstants.OPTIONAL_SHIPPING_INFORMATION, StringUtility.toUpperCase(cargoInfoRows.getShippingInformation()));
                dictionary.put(ReportConstants.OPTIONAL_SHIPPING_INFORMATION_OTHER, StringUtility.toUpperCase(cargoInfoRows.getShippingInformationOther()));
                dictionary.put(ReportConstants.OTHER_INFORMATION, StringUtility.toUpperCase(cargoInfoRows.getOtherInfo()));
                BigDecimal amountOfInsurance = cargoInfoRows.getInsuranceAmount();
                BigDecimal carriageValue = cargoInfoRows.getCarriageValue();
                BigDecimal customsValue = cargoInfoRows.getCustomsValue();
                BigDecimal zeroDecimal = BigDecimal.ZERO;
                if(amountOfInsurance != null && !Objects.equals(amountOfInsurance, zeroDecimal)) {
                    dictionary.put(ReportConstants.AOI, IReport.twoDecimalPlacesFormatDecimal(amountOfInsurance));
                    dictionary.put(ReportConstants.AMOUNT_OF_INSURANCE, AmountNumberFormatter.Format(amountOfInsurance, cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                } else {
                    dictionary.put(ReportConstants.AMOUNT_OF_INSURANCE, "XXX");
                    dictionary.put(ReportConstants.AOI , "XXX");
                }
                if(carriageValue != null && !Objects.equals(carriageValue, zeroDecimal)) {
                    dictionary.put(ReportConstants.DECLARED_VALUE_FOR_CARRIAGE,  AmountNumberFormatter.Format(carriageValue, cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                } else {
                    dictionary.put(ReportConstants.DECLARED_VALUE_FOR_CARRIAGE,  "NVD");
                }
                if(customsValue!= null && !Objects.equals(customsValue, zeroDecimal)) {
                    dictionary.put(ReportConstants.DECLARED_VALUE_FOR_CUSTOMS, AmountNumberFormatter.Format(customsValue, cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                } else {
                    dictionary.put(ReportConstants.DECLARED_VALUE_FOR_CUSTOMS, "NCV");
                }
                dictionary.put(ReportConstants.CURRENCY, cargoInfoRows.getCurrency());
                dictionary.put(ReportConstants.CHARGE_CODE, cargoInfoRows.getChargeCode());
                dictionary.put(ReportConstants.ACCOUNTING_INFORMATION,  StringUtility.toUpperCase(cargoInfoRows.getAccountingInfo()));
                dictionary.put(ReportConstants.HANDLING_INFORMATION, StringUtility.toUpperCase(cargoInfoRows.getHandlingInfo()));
                NtrQtyGoods = cargoInfoRows.getNtrQtyGoods();
                dictionary.put(ReportConstants.NATURE_OF_GOODS, NtrQtyGoods);
                dictionary.put(ReportConstants.SCI, cargoInfoRows.getSci());
                if(StringUtility.isNotEmpty(cargoInfoRows.getChargeCode()))
                    masterDataQuery.add(MasterDataType.PAYMENT_CODES.getDescription() + "#" + cargoInfoRows.getChargeCode());

                dictionary.put(CSD_INFO, cargoInfoRows.getCsdInfo());
                if(StringUtility.isNotEmpty(cargoInfoRows.getCsdInfo()))
                    dictionary.put(ORIGINAL_PRINT_DATE, convertToDPWDateFormatWithTime(hawbModel.getAwb().getOriginalPrintedAt(), v1TenantSettingsResponse.getDPWDateFormat(), true, true));
                dictionary.put(SLAC, cargoInfoRows.getSlac());
                dictionary.put(OTHER_INFO_CODE, cargoInfoRows.getChargeCode());
            }
            List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfo = hawbModel.awb.getAwbGoodsDescriptionInfo();
            List<AwbPackingInfo> awbPackingInfo = hawbModel.awb.getAwbPackingInfo();
            AtomicInteger TotalPieces = new AtomicInteger();
            final BigDecimal[] TotalGrossWeight = {BigDecimal.ZERO};
            final BigDecimal[] SumOfTotalAmount = {BigDecimal.ZERO};
            final BigDecimal[] SumOfChargeableWt = {BigDecimal.ZERO};
            String FreightAmountText = "";
            String OtherAmountText = "";

            masterDataQuery.add(MasterDataType.MAWB_CHARGE_TEXT.getDescription() + "#" + AwbConstants.FREIGHT_AMOUNT);
            masterDataQuery.add(MasterDataType.MAWB_CHARGE_TEXT.getDescription() + "#" + AwbConstants.OTHER_AMOUNT);

            if (originAirport != null && originAirport.getCountry() != null)
            {
                masterDataQuery.add(MasterDataType.COUNTRIES.getDescription() + "#" + originAirport.getCountry());
                dictionary.put(ReportConstants.AIRPORT_OF_DEPARTURE , upperCase(originAirport.getName()));
            }
            if (destinationAirport != null && destinationAirport.getCountry() != null)
            {
                masterDataQuery.add(MasterDataType.COUNTRIES.getDescription() + "#" + destinationAirport.getCountry());
                dictionary.put(ReportConstants.AIRPORT_OF_DESTINATION, upperCase(destinationAirport.getName()));
            }

            Map<String, EntityTransferMasterLists> dataMap = getMasterData(masterDataQuery);

            if(dataMap != null) {
                if(dataMap.get(MasterDataType.MAWB_CHARGE_TEXT.getDescription() + "#" + AwbConstants.FREIGHT_AMOUNT) != null) {
                    FreightAmountText = dataMap.get(MasterDataType.MAWB_CHARGE_TEXT.getDescription() + "#" + AwbConstants.FREIGHT_AMOUNT).ItemDescription.toUpperCase();
                }
                if(dataMap.get(MasterDataType.MAWB_CHARGE_TEXT.getDescription() + "#" + AwbConstants.OTHER_AMOUNT) != null) {
                    OtherAmountText = dataMap.get(MasterDataType.MAWB_CHARGE_TEXT.getDescription() + "#" + AwbConstants.OTHER_AMOUNT).ItemDescription.toUpperCase();
                }
                paymentCodeDetails = dataMap.get(MasterDataType.PAYMENT_CODES.getDescription() + "#" + cargoInfoRows.getChargeCode());

                if (hawbModel.getEntityType().equalsIgnoreCase(AwbConstants.MAWB))
                {
                    if (StringUtility.isNotEmpty(consolRow.getPayment()))
                    {
                        paymentTerms = dataMap.get(MasterDataType.PAYMENT.getDescription() + "#" + consolRow.getPayment());
                        if (paymentTerms != null)
                        {
                            dictionary.put(ReportConstants.PAYMENT_TERMS, paymentTerms.ItemDescription);
                        }
                    }
                }
                else
                {
                    if (StringUtility.isNotEmpty(hawbModel.shipmentDetails.getPaymentTerms()))
                    {
                        paymentTerms = dataMap.get(MasterDataType.PAYMENT.getDescription() + "#" + hawbModel.shipmentDetails.getPaymentTerms());
                        if (paymentTerms != null)
                        {
                            dictionary.put(ReportConstants.PAYMENT_TERMS, paymentTerms.ItemDescription);
                        }
                    }
                }

                if (originAirport != null && dataMap.get(MasterDataType.COUNTRIES.getDescription() + "#" + originAirport.getCountry()) != null)
                {
                    dictionary.put(ReportConstants.DEPARTURE_AIRPORT_COUNTRY, dataMap.get(MasterDataType.COUNTRIES.getDescription() + "#" + originAirport.getCountry()).ItemDescription.toUpperCase());
                }

                if (destinationAirport != null && dataMap.get(MasterDataType.COUNTRIES.getDescription() + "#" + destinationAirport.getCountry()) != null)
                {
                    dictionary.put(ReportConstants.DESTINATION_AIRPORT_COUNTRY, dataMap.get(MasterDataType.COUNTRIES.getDescription() + "#" + destinationAirport.getCountry()).ItemDescription.toUpperCase());
                }
            }


            dictionary.put(ReportConstants.FREIGHT_AMOUNT_TEXT,  FreightAmountText);
            dictionary.put(ReportConstants.OTHER_AMOUNT_TEXT, OtherAmountText);
            Set<String> hsCodesSet = new HashSet<>();
            Set<String> slacCodeSet = new HashSet<>();
            if (awbPackingInfo != null && !awbPackingInfo.isEmpty()) {
                awbPackingInfo.forEach(packInfo -> {
                    if (packInfo.getHsCode() != null && !packInfo.getHsCode().isEmpty())
                        hsCodesSet.add(packInfo.getHsCode());
                });
            }
            if (awbGoodsDescriptionInfo != null && awbGoodsDescriptionInfo.size() > 0){
                String finalNtrQtyGoods = NtrQtyGoods;
                List<AwbGoodsDescriptionInfoModel> awbGoodsDescriptionInfoModel = awbGoodsDescriptionInfo.stream().map(x ->modelMapper.map(x, AwbGoodsDescriptionInfoModel.class)).toList();
                List<Map<String,Object>> values = jsonHelper.convertValue(awbGoodsDescriptionInfoModel, new TypeReference<>(){});
                List<Map<String,Object>> valuesFAT = jsonHelper.convertValue(values, new TypeReference<>(){});
                values.forEach(value -> {
                    value.put(ReportConstants.NATURE_QLTY_OF_GOODS, finalNtrQtyGoods);
                    if(value.get(ReportConstants.RATE_CLASS) != null){
                        value.put(ReportConstants.RATE_CLASS, RateClass.getById((Integer) value.get(ReportConstants.RATE_CLASS)));
                    }
                    if(value.get(GROSS_WT_UNIT) != null){
                        value.put(GROSS_WT_UNIT, convertToSingleCharWeightFormat((String) value.get(GROSS_WT_UNIT)));
                    }
                    if(value.get(RCP) != null){
                        value.put(RCP, value.get(RCP).toString());
                    }
                    if(value.get(ReportConstants.GROSS_WT) != null){
                        value.put(ReportConstants.GROSS_WT, ConvertToWeightNumberFormat(value.get(ReportConstants.GROSS_WT).toString(), v1TenantSettingsResponse));
                    }
                    if(value.get(ReportConstants.CHARGEABLE_WT) != null){
                        value.put(ReportConstants.CHARGEABLE_WT, ConvertToWeightNumberFormat(value.get(ReportConstants.CHARGEABLE_WT).toString(), CHARGEABLE_WEIGHT_DECIMAL_PLACES, v1TenantSettingsResponse));
                    }
                    if(value.get(ReportConstants.RATE_CHARGE) != null){
                        value.put(ReportConstants.RATE_CHARGE, AmountNumberFormatter.Format(new BigDecimal(value.get(ReportConstants.RATE_CHARGE).toString()), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                    } else {
                        value.put(ReportConstants.RATE_CHARGE, IReport.addCommas(0));
                    }
                    if(value.get(ReportConstants.TOTAL_AMOUNT) != null){
                        value.put(ReportConstants.TOTAL_AMOUNT, AmountNumberFormatter.Format(new BigDecimal(value.get(ReportConstants.TOTAL_AMOUNT).toString()), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                    } else {
                        value.put(ReportConstants.TOTAL_AMOUNT, IReport.addCommas(0));
                    }
                    if(value.get(PIECES_NO) != null)
                        value.put(PIECES_NO, GetDPWWeightVolumeFormat(new BigDecimal(value.get(PIECES_NO).toString()), 0, v1TenantSettingsResponse));
                    if (value.get(HS_CODE1) != null) {
                        String hsCode = value.get(HS_CODE1).toString();
                        if (!hsCode.isEmpty())
                            hsCodesSet.add(hsCode);
                    }
                    if (value.get(SLAC_CODE) != null) {
                        String slacCode = value.get(SLAC_CODE).toString();
                        if (!slacCode.isEmpty())
                            slacCodeSet.add(slacCode);
                    }
                });
                dictionary.put(ReportConstants.PACKING_LIST, values);
                String finalFreightAmountText = FreightAmountText;
                valuesFAT.forEach(value -> {
                    value.put(ReportConstants.NATURE_QLTY_OF_GOODS, finalNtrQtyGoods);
                    if(value.get(ReportConstants.RATE_CLASS) != null){
                        value.put(ReportConstants.RATE_CLASS, RateClass.getById((Integer) value.get(ReportConstants.RATE_CLASS)));
                    }
                    if(value.get(ReportConstants.GROSS_WT) != null){
                        value.put(ReportConstants.GROSS_WT, ConvertToWeightNumberFormat(value.get(ReportConstants.GROSS_WT).toString(), v1TenantSettingsResponse));
                    }
                    if(value.get(ReportConstants.CHARGEABLE_WT) != null){
                        value.put(ReportConstants.CHARGEABLE_WT, ConvertToWeightNumberFormat(value.get(ReportConstants.CHARGEABLE_WT).toString(), CHARGEABLE_WEIGHT_DECIMAL_PLACES, v1TenantSettingsResponse));
                    }
                    if (value.get(ReportConstants.RATE_CHARGE) != null)
                    {
                        value.put(ReportConstants.RATE_CHARGE, finalFreightAmountText);
                    }
                    if (value.get(ReportConstants.TOTAL_AMOUNT) != null)
                    {
                        value.put(ReportConstants.TOTAL_AMOUNT, finalFreightAmountText);
                    }
                });
                dictionary.put(ReportConstants.PACKING_LIST_FAT, valuesFAT);
                awbGoodsDescriptionInfo.forEach(row -> {
                    TotalPieces.addAndGet((row.getPiecesNo() != null ? row.getPiecesNo() : 0));
                    TotalGrossWeight[0] =  TotalGrossWeight[0].add(row.getGrossWt() != null ? row.getGrossWt() : BigDecimal.ZERO);
                    SumOfTotalAmount[0] = SumOfTotalAmount[0].add(row.getTotalAmount() != null ? row.getTotalAmount() : BigDecimal.ZERO);
                    SumOfChargeableWt[0] = SumOfChargeableWt[0].add(row.getChargeableWt() != null ? row.getChargeableWt() : BigDecimal.ZERO);
                });
                dictionary.put(ReportConstants.TOtAl_PIECES, GetDPWWeightVolumeFormat(BigDecimal.valueOf(TotalPieces.get()), 0, v1TenantSettingsResponse));
                dictionary.put(ReportConstants.TOTAL_GROSS_WEIGHT, ConvertToWeightNumberFormat(TotalGrossWeight[0], v1TenantSettingsResponse));
                dictionary.put(ReportConstants.TGW, ConvertToWeightNumberFormat(TotalGrossWeight[0], v1TenantSettingsResponse));
                dictionary.put(ReportConstants.SUM_OF_TOTAL_AMOUNT, AmountNumberFormatter.Format(SumOfTotalAmount[0], cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                dictionary.put(ReportConstants.SUM_OF_TOTAL_AMOUNT_FAT, FreightAmountText);
                dictionary.put(ReportConstants.SUM_OF_CHARGEABLE_WT, ConvertToWeightNumberFormat(SumOfChargeableWt[0], v1TenantSettingsResponse));
            }

            if (!hsCodesSet.isEmpty()) {
                String commaHsCode = HSCODE + ": ";
                commaHsCode += String.join(", ", hsCodesSet);
                dictionary.put(COMMA_HS_CODE1, commaHsCode);
            }
            if (!slacCodeSet.isEmpty()) {
                String commaSlacCode = SLAC + ": ";
                commaSlacCode += String.join(", ", slacCodeSet);
                dictionary.put(COMMA_SLAC1, commaSlacCode);
            }

            List<AwbRoutingInfo> routingInfoRows = hawbModel.awb.getAwbRoutingInfo();
            Set<String> carrierSet;
            String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
            if(routingInfoRows != null && routingInfoRows.size() > 0){
                locCodes = new HashSet<>();
                carrierSet = new HashSet<>();

                locCodes.add(routingInfoRows.get(0).getDestinationPortName());
                locCodes.add(routingInfoRows.get(0).getOriginPortName());
                carrierSet.add(routingInfoRows.get(0).getByCarrier());

                if(routingInfoRows.size() >= 2) {
                    locCodes.add(routingInfoRows.get(1).getDestinationPortName());
                    carrierSet.add(routingInfoRows.get(1).getByCarrier());
                }
                if(routingInfoRows.size() >= 3) {
                    locCodes.add(routingInfoRows.get(2).getDestinationPortName());
                    carrierSet.add(routingInfoRows.get(2).getByCarrier());
                }
                // Fetch all the possible loc codes possible in single call
                locCodeMap = getLocationData(locCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
                // Fetch all the possible carrier data in single call
                Map<String, EntityTransferCarrier> carrierRow = fetchCarrier(carrierSet);

                dictionary.put(ReportConstants.TO_FIRST, locCodeMap.get(routingInfoRows.get(0).getDestinationPortName()) != null ? locCodeMap.get(routingInfoRows.get(0).getDestinationPortName()).getIataCode() : null);
                dictionary.put(ReportConstants.TO, dictionary.get(ReportConstants.TO_FIRST));
                dictionary.put(ReportConstants.AO_DEPT_CODE, locCodeMap.get(routingInfoRows.get(0).getOriginPortName()) != null ? locCodeMap.get(routingInfoRows.get(0).getOriginPortName()).getIataCode() : null);
                dictionary.put(ReportConstants.ISSUED_BY, carrierDetailModel.getShippingLine());
                dictionary.put(ReportConstants.FLIGHT_NO1, routingInfoRows.get(0).getFlightNumber());
                dictionary.put(ReportConstants.FLIGHT_DATE1, ConvertToDPWDateFormat(routingInfoRows.get(0).getFlightDate(), tsDateTimeFormat));

                String carrier = routingInfoRows.get(0).getByCarrier();
                String carrierCode = carrier != null && carrierRow.containsKey(carrier) ? carrierRow.get(carrier).IATACode : "";
                String flightNumber = routingInfoRows.get(0).getFlightNumber();
                String day = routingInfoRows.get(0).getFlightDate() != null ? String.valueOf(routingInfoRows.get(0).getFlightDate().getDayOfMonth()) : "";
                dictionary.put(ReportConstants.FIRST_FLIGHT_AND_DAY, String.format("%s%s/%s", carrierCode, flightNumber, day));
                dictionary.put(ReportConstants.BY_FIRST_CARRIER_NAME, carrier);

                if (!carrierRow.isEmpty() && carrier != null && carrierRow.containsKey(carrier))
                {
                    dictionary.put(ReportConstants.BY_FIRST, carrierRow.get(carrier).IATACode);
                    dictionary.put(ISSUED_BY_NAME_IN_CAPS, (carrierRow.get(carrier).ItemDescription).toUpperCase());
                }
                else
                {
                    dictionary.put(ReportConstants.BY_FIRST, "");
                }
                dictionary.put(ReportConstants.BY, dictionary.get(ReportConstants.BY_FIRST));

                List<String> flightNumberList = new ArrayList<>();
                List<String> flightDateList = new ArrayList<>();
                flightNumberList.add(String.format(REGEX_S_S, dictionary.get(ReportConstants.BY_FIRST), dictionary.get(ReportConstants.FLIGHT_NO1)));
                flightDateList.add(ConvertToDPWDateFormat(routingInfoRows.get(0).getFlightDate(), tsDateTimeFormat));


                if(routingInfoRows.size()>=2){
//                    locCodes.add(routingInfoRows.get(1).getDestination());
//                    locCodeMap = getLocationData(locCodes);
                    if(locCodeMap.containsKey(routingInfoRows.get(1).getDestinationPortName()))
                        dictionary.put(ReportConstants.TO_SECOND, locCodeMap.get(routingInfoRows.get(1).getDestinationPortName()).getIataCode());
//                    carrierSet.add(routingInfoRows.get(1).getByCarrier());
//                    carrierRow = fetchCarrier(carrierSet);
                    if (carrierRow.containsKey(routingInfoRows.get(1).getByCarrier()))
                    {
                        dictionary.put(ReportConstants.BY_SECOND, carrierRow.get(routingInfoRows.get(1).getByCarrier()).IATACode);
                    }
                    dictionary.put(ReportConstants.FLIGHT_NO2, routingInfoRows.get(1).getFlightNumber());
                    dictionary.put(ReportConstants.FLIGHT_DATE2, ConvertToDPWDateFormat(routingInfoRows.get(1).getFlightDate(), tsDateTimeFormat));
                    flightNumberList.add(String.format(REGEX_S_S, dictionary.get(ReportConstants.BY_SECOND), dictionary.get(ReportConstants.FLIGHT_NO2)));
                    flightDateList.add(ConvertToDPWDateFormat(routingInfoRows.get(1).getFlightDate(), tsDateTimeFormat));
                    String carrier2 = routingInfoRows.get(1).getByCarrier();
                    String carrierCode2 = carrier2 != null && carrierRow.containsKey(carrier2) ? carrierRow.get(carrier2).IATACode : "";
                    String flightNumber2 = routingInfoRows.get(1).getFlightNumber();
                    String day2 = routingInfoRows.get(1).getFlightDate() != null ? String.valueOf(routingInfoRows.get(1).getFlightDate().getDayOfMonth()) : "";
                    dictionary.put(ReportConstants.SECOND_FLIGHT_AND_DAY, String.format("%s%s/%s", carrierCode2, flightNumber2, day2));
                }
                if(routingInfoRows.size()>=3){
                    locCodes = new HashSet<>();
                    locCodes.add(routingInfoRows.get(2).getDestinationPortName());
//                    locCodeMap = getLocationData(locCodes);
                    if(locCodeMap.containsKey(routingInfoRows.get(2).getDestinationPortName()))
                        dictionary.put(ReportConstants.TO_THIRD, locCodeMap.get(routingInfoRows.get(2).getDestinationPortName()).getIataCode());
//                    carrierSet.add(routingInfoRows.get(2).getByCarrier());
//                    carrierRow = fetchCarrier(carrierSet);
                    if (carrierRow.containsKey(routingInfoRows.get(1).getByCarrier()))
                    {
                        dictionary.put(ReportConstants.BY_THIRD, carrierRow.get(routingInfoRows.get(2).getByCarrier()).IATACode);
                    }
                    dictionary.put(ReportConstants.FLIGHT_NO3, routingInfoRows.get(2).getFlightNumber());
                    dictionary.put(ReportConstants.FLIGHT_DATE3, ConvertToDPWDateFormat(routingInfoRows.get(2).getFlightDate(), tsDateTimeFormat));
                    flightNumberList.add(String.format(REGEX_S_S, dictionary.get(ReportConstants.BY_THIRD), dictionary.get(ReportConstants.FLIGHT_NO3)));
                    flightDateList.add(ConvertToDPWDateFormat(routingInfoRows.get(2).getFlightDate(), tsDateTimeFormat));
                }

                dictionary.put(ReportConstants.FLIGHT_NO, String.join(",", flightNumberList));
                dictionary.put(ReportConstants.FLIGHT_DATE, String.join(",", flightDateList));
            }
            AwbPaymentInfo paymentInfoRows = hawbModel.awb.getAwbPaymentInfo();

            dictionary.put(ReportConstants.TOTAL_COLLECT, null);
            dictionary.put(ReportConstants.TOTAL_PREPAID, null);
            dictionary.put(ReportConstants.WT_CHARGE_P, null);
            dictionary.put(ReportConstants.VALUATION_CHARGES_P, null);
            dictionary.put(ReportConstants.TAX_P, null);
            dictionary.put(ReportConstants.AGENT_DUE_P, null);
            dictionary.put(ReportConstants.CARRIER_DUE_P, null);
            dictionary.put(ReportConstants.AGENT_DUE_POAT, null);
            dictionary.put(ReportConstants.CARRIER_DUE_POAT, null);
            dictionary.put(ReportConstants.WT_CHARGE_C, null);
            dictionary.put(ReportConstants.WT_CHARGE_CFAT, null);
            dictionary.put(ReportConstants.VALUATION_CHARGES_C, null);
            dictionary.put(ReportConstants.TAX_C, null);
            dictionary.put(ReportConstants.AGENT_DUE_C, null);
            dictionary.put(ReportConstants.CARRIER_DUE_C, null);
            dictionary.put(ReportConstants.FREIGHT_AMOUNT_TEXT_C, null);
            dictionary.put(ReportConstants.OTHER_AMOUNT_TEXT_C, null);
            dictionary.put(ReportConstants.FREIGHT_AMOUNT_TEXT_P, null);
            dictionary.put(ReportConstants.OTHER_AMOUNT_TEXT_P, null);
            dictionary.put(ReportConstants.TOTAL_FREIGHT_P, null);
            dictionary.put(ReportConstants.TOTAL_FREIGHT_C, null);
            dictionary.put(ReportConstants.TOTAL_OTHERS_P, null);
            dictionary.put(ReportConstants.TOTAL_OTHERS_C, null);
            if(paymentInfoRows != null){
                if(!BigDecimal.ZERO.equals(paymentInfoRows.getTotalCollect()))
                    dictionary.put(ReportConstants.TOTAL_COLLECT, AmountNumberFormatter.Format(paymentInfoRows.getTotalCollect(), cargoInfoRows != null ? cargoInfoRows.getCurrency() : null, v1TenantSettingsResponse));
                if(!BigDecimal.ZERO.equals(paymentInfoRows.getTotalPrepaid()))
                    dictionary.put(ReportConstants.TOTAL_PREPAID, AmountNumberFormatter.Format(paymentInfoRows.getTotalPrepaid(), cargoInfoRows != null ? cargoInfoRows.getCurrency() : null, v1TenantSettingsResponse));
                dictionary.put(ReportConstants.AGENT_DUE_POAT, OtherAmountText);
                dictionary.put(ReportConstants.CARRIER_DUE_POAT, OtherAmountText);
                dictionary.put(ReportConstants.WT_CHARGE_CFAT, FreightAmountText);
                if (paymentCodeDetails != null)
                {
                    BigDecimal totalOthers = BigDecimal.ZERO;
                    BigDecimal totalFreight = SumOfTotalAmount[0];
                    if (paymentInfoRows.getDueAgentCharges() != null)
                    {
                        totalOthers = paymentInfoRows.getDueAgentCharges();
                    }
                    if (paymentInfoRows.getDueCarrierCharges() != null)
                    {
                        totalOthers = totalOthers.add(paymentInfoRows.getDueCarrierCharges());
                    }
                    if (paymentInfoRows.getWeightCharges() != null)
                    {
                        totalFreight = paymentInfoRows.getWeightCharges();
                    }
                    if (paymentInfoRows.getTax() != null)
                    {
                        totalFreight = totalFreight.add(paymentInfoRows.getTax());
                    }
                    if (paymentInfoRows.getValuationCharge() != null)
                    {
                        totalFreight = totalFreight.add(paymentInfoRows.getValuationCharge());
                    }

                    if (paymentCodeDetails.Identifier1 != null && paymentCodeDetails.Identifier1.equalsIgnoreCase("true"))
                    {
                        dictionary.put(ReportConstants.WTVALP, "X");
                        dictionary.put(ReportConstants.WT_CHARGE_P, AmountNumberFormatter.Format(paymentInfoRows.getWeightCharges(), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.VALUATION_CHARGES_P, AmountNumberFormatter.Format(paymentInfoRows.getValuationCharge(), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.TAX_P, AmountNumberFormatter.Format(paymentInfoRows.getTax(), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.TOTAL_FREIGHT_P, AmountNumberFormatter.Format(totalFreight, cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.FREIGHT_AMOUNT_TEXT_P, FreightAmountText);
                    }
                    if (paymentCodeDetails.Identifier2 != null && paymentCodeDetails.Identifier2.equalsIgnoreCase("true"))
                    {
                        dictionary.put(ReportConstants.WTVALC, "X");
                        dictionary.put(ReportConstants.WT_CHARGE_C, AmountNumberFormatter.Format(paymentInfoRows.getWeightCharges(), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.VALUATION_CHARGES_C, AmountNumberFormatter.Format(paymentInfoRows.getValuationCharge(), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.TAX_C, AmountNumberFormatter.Format(paymentInfoRows.getTax(), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.TOTAL_FREIGHT_C, AmountNumberFormatter.Format(totalFreight, cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.FREIGHT_AMOUNT_TEXT_C, FreightAmountText);
                    }
                    if (paymentCodeDetails.Identifier3 != null && paymentCodeDetails.Identifier3.equalsIgnoreCase("true"))
                    {
                        dictionary.put(ReportConstants.OTHERS_P, "X");
                        dictionary.put(ReportConstants.AGENT_DUE_P, AmountNumberFormatter.Format(paymentInfoRows.getDueAgentCharges(), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.CARRIER_DUE_P, AmountNumberFormatter.Format(paymentInfoRows.getDueCarrierCharges(), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.TOTAL_OTHERS_P, AmountNumberFormatter.Format(totalOthers, cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.OTHER_AMOUNT_TEXT_P, OtherAmountText);
                    }
                    if (paymentCodeDetails.Identifier4 != null && paymentCodeDetails.Identifier4.equalsIgnoreCase("true"))
                    {
                        dictionary.put(ReportConstants.OTHERS_C, "X");
                        dictionary.put(ReportConstants.AGENT_DUE_C, AmountNumberFormatter.Format(paymentInfoRows.getDueAgentCharges(), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.CARRIER_DUE_C, AmountNumberFormatter.Format(paymentInfoRows.getDueCarrierCharges(), cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.TOTAL_OTHERS_C, AmountNumberFormatter.Format(totalOthers, cargoInfoRows.getCurrency(), v1TenantSettingsResponse));
                        dictionary.put(ReportConstants.OTHER_AMOUNT_TEXT_C, OtherAmountText);
                    }
                }
            }

            if(StringUtility.isNotEmpty(AwbNumber)){
                AwbNumber = AwbNumber.replace("-","");
                if(AwbNumber.length() < 11)
                {
                    AwbNumber = IReport.appendZero(AwbNumber, 11);
                }
                dictionary.put(ReportConstants.MAWB_NUMBER, AwbNumber);
            }else{
                dictionary.put(ReportConstants.MAWB_NUMBER,null);
            }

            AwbOtherInfo otherInfoRows = hawbModel.awb.getAwbOtherInfo();
            if(otherInfoRows != null)
            {
                locCodes = new HashSet<>();
                locCodes.add(otherInfoRows.getExecutedAt());
                locCodeMap = getLocationData(locCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
                String executedAtName = null;
                if (locCodeMap.get(otherInfoRows.getExecutedAt()) != null) {
                    // Get the name from v1 and convert it to uppercase if not null
                    executedAtName = Optional.ofNullable(locCodeMap.get(otherInfoRows.getExecutedAt()).getName())
                            .map(String::toUpperCase)
                            .orElse(null);
                }
                dictionary.put(ReportConstants.EXECUTED_AT, locCodeMap.get(otherInfoRows.getExecutedAt()) != null ?  locCodeMap.get(otherInfoRows.getExecutedAt()).getIataCode() : null);
                dictionary.put(ReportConstants.EXECUTED_AT_NAME, executedAtName);
                dictionary.put(ReportConstants.EXECUTED_ON, ConvertToDPWDateFormat(otherInfoRows.getExecutedOn(), tsDateTimeFormat, true));
                dictionary.put(ReportConstants.SIGN_OF_SHIPPER, otherInfoRows.getShipper());
                dictionary.put(ReportConstants.SIGN_OF_ISSUING_CARRIER, StringUtility.toUpperCase(otherInfoRows.getCarrier()));
                dictionary.put(ReportConstants.BRANCH_NAME, StringUtility.toUpperCase(otherInfoRows.getBranch()));
                dictionary.put(ReportConstants.LEGAL_COMPANY_NAME, StringUtility.toUpperCase(otherInfoRows.getLegalCompanyName()));
                List<String> companyAddress = ReportHelper.getOrgAddress(otherInfoRows.getAddress1(), otherInfoRows.getAddress2(), otherInfoRows.getState(), otherInfoRows.getCity(), otherInfoRows.getCountryCode(), otherInfoRows.getPincode());
                companyAddress.add(otherInfoRows.getCountryName());
                companyAddress = companyAddress.stream().map(StringUtility::toUpperCase).toList();
                dictionary.put(ReportConstants.COMPANY_ADDRESS, companyAddress);
                dictionary.put(ReportConstants.ISSUED_BY_NAME, StringUtility.toUpperCase(otherInfoRows.getCarrierName()));
                dictionary.put(CARRIER_HQ, StringUtility.toUpperCase(otherInfoRows.getCarrierHqAddress()));
            }

            List<AwbOtherChargesInfo> otherChargesInfoRows = hawbModel.awb.getAwbOtherChargesInfo();
            dictionary.put(ReportConstants.OTHER_CHARGES, getOtherChargesDetails(otherChargesInfoRows, hawbModel.awb, cargoInfoRows, v1TenantSettingsResponse).getOtherChargesItems());
            dictionary.put(ReportConstants.NEW_OTHER_CHARGES, getOtherChargesDetails(otherChargesInfoRows,hawbModel.awb, cargoInfoRows, v1TenantSettingsResponse).getNewOtherChargesItems());
            dictionary.put(ReportConstants.OTHER_CHARGES_IATA, getOtherChargesDetailsIATA(otherChargesInfoRows, hawbModel.awb, v1TenantSettingsResponse, cargoInfoRows).getOtherChargesItems());
            dictionary.put(ReportConstants.NEW_OTHER_CHARGES_IATA, getOtherChargesDetailsIATA(otherChargesInfoRows, hawbModel.awb, v1TenantSettingsResponse, cargoInfoRows).getNewOtherChargesItems());
            dictionary.put(ReportConstants.IATA_DESCRIPTION, getIATADescription(otherChargesInfoRows));
            dictionary.put(ReportConstants.OTHER_CHARGES_OAT, getOtherChargesDetailsOAT(otherChargesInfoRows,OtherAmountText));
            dictionary.put(ReportConstants.OTHER_CHARGES_IATA_OAT, getOtherChargesDetailsIATAOAT(otherChargesInfoRows, OtherAmountText));
            List<AwbSpecialHandlingCodesMappingInfo> specialHandlingCodesRows = hawbModel.awb.getAwbSpecialHandlingCodesMappings();
            dictionary.put(ReportConstants.SPECIAL_HANDLING_CODE, getSpecialHandlingCodes(specialHandlingCodesRows));
            if (!Objects.isNull(hawbModel.getShipmentDetails())) {
                dictionary.put(PICKUP_INSTRUCTION, hawbModel.getShipmentDetails().getPickupDetails() != null ? hawbModel.getShipmentDetails().getPickupDetails().getPickupDeliveryInstruction() : null);
                dictionary.put(DELIVERY_INSTRUCTIONS, hawbModel.getShipmentDetails().getDeliveryDetails() != null ? hawbModel.getShipmentDetails().getDeliveryDetails().getPickupDeliveryInstruction() : null);
            }
        }

        AirMessagingAdditionalFields airMessagingAdditionalFields = hawbModel.awb.getAirMessagingAdditionalFields();

        if(airMessagingAdditionalFields != null){
            dictionary.put(TARGET_CURRENCY_CODE, airMessagingAdditionalFields.getTargetCurrencyCode());
            dictionary.put(CONVERSION_RATE, airMessagingAdditionalFields.getConversionRate());
            dictionary.put(CC_CHARGE_IN_DEST_CURRENCY, airMessagingAdditionalFields.getCCChargesInDestinationCurrency());
            dictionary.put(CHARGES_AT_DESTINATION, airMessagingAdditionalFields.getChargesAtDestination());
        }
        
        if(!Objects.equals(hawbModel.shipmentDetails, null)) {
            populateRaKcData(dictionary, hawbModel.shipmentDetails);
            populateShipmentOrders(hawbModel.shipmentDetails, dictionary);
        }

        var awbNotifParty = hawbModel.getAwb().getAwbNotifyPartyInfo();

        if(!CommonUtils.listIsNullOrEmpty(awbNotifParty)) {
            var party = hawbModel.getAwb().getAwbNotifyPartyInfo().get(0);
            dictionary.put(NOTIFY_PARTY, getAwbFormattedDetails(party.getName(), party.getAddress(), party.getCity(), party.getState(), party.getZipCode(), party.getCountry(), party.getContactName(), party.getPhone(), party.getTaxRegistrationNumber()));
            dictionary.put(AWB_NOTIFYPARTY, getFormattedDetails(party.getName(), party.getAddress(), party.getCountry(), party.getState(), party.getCity(), party.getZipCode(), party.getPhone()));
            dictionary.put(AWB_NOTIFY_PARTY_NAME, (party.getName() != null && !party.getName().isEmpty()) ?  "Notify: " + hawbModel.getAwb().getAwbNotifyPartyInfo().get(0).getName() : "");
        }

        return dictionary;
    }

    public static String constructAddressForAwb(String address, String country, String state, String city, String zipCode, String phone) {
        StringBuilder sb = new StringBuilder();
        String newLine = "\r\n";
        if(address != null) {
            sb.append(address);
        }

        StringBuilder tempAddress = new StringBuilder();
        if (!Strings.isNullOrEmpty(state)){
            tempAddress.append(state);
        }
        if (!Strings.isNullOrEmpty(city)){
            if(!tempAddress.isEmpty())
                tempAddress.append(", ");
            tempAddress.append(city);
        }
        if (!Strings.isNullOrEmpty(country)){
            if(!tempAddress.isEmpty())
                tempAddress.append(", ");
            tempAddress.append(country);
        }
        if(!tempAddress.isEmpty()) {
            if(!sb.isEmpty())
                sb.append(newLine);
            sb.append(tempAddress);
        }

        if (!Strings.isNullOrEmpty(zipCode)){
            if(!sb.isEmpty())
                sb.append(newLine);
            sb.append(zipCode);
        }
        if (!Strings.isNullOrEmpty(phone)){
            if(!sb.isEmpty())
                sb.append(newLine);
            sb.append(phone);
        }
        return sb.toString();
    }

    public static OtherChargesResponse getOtherChargesDetails(List<AwbOtherChargesInfo> otherChargesRows, Awb siData, AwbCargoInfo cargoInfoRows, V1TenantSettingsResponse v1TenantSettingsResponse)
    {
        OtherChargesResponse otherChargesResponses = new OtherChargesResponse();
        Map<String,BigDecimal> carrierCharges = new HashMap<>();
        Map<String,BigDecimal> agentCharges = new HashMap<>();
        List<String> newOtherChargesList = new ArrayList<>();

        for (AwbOtherChargesInfo chargeRow : emptyIfNull(otherChargesRows))
        {
            ChargesDue chargeDue = ChargesDue.getById(chargeRow.getChargeDue());
            String chargeKey = chargeRow.getChargeTypeId();
            BigDecimal chargeAmount = (chargeRow.getAmount() != null ? chargeRow.getAmount() : BigDecimal.ZERO);
            String newOtherCharges = chargeKey + " : " + (siData.getAwbCargoInfo() != null ? siData.getAwbCargoInfo().getCurrency() : "") + " " + AmountNumberFormatter.Format(chargeAmount, cargoInfoRows.getCurrency(), v1TenantSettingsResponse);
            if (chargeDue == ChargesDue.AGENT) {
                if(agentCharges.containsKey(chargeKey)) {
                    agentCharges.put(chargeKey, agentCharges.get(chargeKey).add(chargeAmount));
                } else {
                    agentCharges.put(chargeKey, chargeAmount);
                }
            } else {
                if(carrierCharges.containsKey(chargeKey)) {
                    carrierCharges.put(chargeKey, carrierCharges.get(chargeKey).add(chargeAmount));
                } else {
                    carrierCharges.put(chargeKey, chargeAmount);
                }
            }
            newOtherChargesList.add(newOtherCharges);
        }

        String CarrierChargesStr = getStringFromDict(carrierCharges, v1TenantSettingsResponse);
        String AgentChargesStr = getStringFromDict(agentCharges, v1TenantSettingsResponse);

        List<String> otherCharges = Arrays.asList(AgentChargesStr, CarrierChargesStr);
        otherChargesResponses.setOtherChargesItems(otherCharges);
        otherChargesResponses.setOtherChargesItems(newOtherChargesList);

        return otherChargesResponses;
    }

    public static List<String> getOtherChargesDetailsOAT(List<AwbOtherChargesInfo> otherChargesRows, String OAT)
    {
        Map<String, String> carrierCharges = new HashMap<>();
        Map<String, String> agentCharges = new HashMap<>();
        StringBuilder AgentChargesStrBuilder = new StringBuilder();
        StringBuilder CarrierChargesStrBuilder = new StringBuilder();
        for (AwbOtherChargesInfo chargeRow : emptyIfNull(otherChargesRows))
        {
            ChargesDue chargeDue = ChargesDue.getById(chargeRow.getChargeDue());
            String chargeKey = chargeRow.getChargeTypeId();
            if (chargeDue == ChargesDue.AGENT)
            {
                if (!agentCharges.containsKey(chargeKey))
                {
                    if (AgentChargesStrBuilder.isEmpty())
                    {
                        AgentChargesStrBuilder.append(chargeKey).append(":").append(OAT);
                    }
                    else
                    {
                        AgentChargesStrBuilder.append(" , ").append(chargeKey).append(":").append(OAT);
                    }
                    agentCharges.put(chargeKey, OAT);
                }
            }
            else
            {
                if (!carrierCharges.containsKey(chargeKey))
                {
                    if (CarrierChargesStrBuilder.isEmpty())
                    {
                        CarrierChargesStrBuilder.append(chargeKey).append(":").append(OAT);
                    }
                    else
                    {
                        CarrierChargesStrBuilder.append(" , ").append(chargeKey).append(":").append(OAT);
                    }
                    carrierCharges.put(chargeKey, OAT);
                }
            }

        }

        List<String> otherCharges = new ArrayList<>();
        otherCharges.add(AgentChargesStrBuilder.toString());
        otherCharges.add(CarrierChargesStrBuilder.toString());
        return otherCharges;
    }

    public static List<String> getOtherChargesDetailsIATAOAT(List<AwbOtherChargesInfo> otherChargesRows, String OAT)
    {
        Map<String, String> carrierCharges = new HashMap<>();
        Map<String, String> agentCharges = new HashMap<>();
        StringBuilder AgentChargesStrBuilder = new StringBuilder();
        StringBuilder CarrierChargesStrBuilder = new StringBuilder();
        for (AwbOtherChargesInfo chargeRow : emptyIfNull(otherChargesRows))
        {
            ChargesDue chargeDue = ChargesDue.getById(chargeRow.getChargeDue());
            String chargeKey;
            if (chargeRow.getIataDescription() != null)
            {
                chargeRow.setIataDescription(chargeRow.getIataDescription().toUpperCase());
                chargeKey = chargeRow.getIataDescription();

                if (chargeDue == ChargesDue.AGENT)
                {
                    if (chargeKey.length() < 3)
                    {
                        chargeKey = chargeKey + Constants.AGENT_PREFIX;
                    }
                    if (!agentCharges.containsKey(chargeKey))
                    {
                        if (AgentChargesStrBuilder.isEmpty())
                        {
                            AgentChargesStrBuilder.append(chargeKey).append(":").append(OAT);
                        }
                        else
                        {
                            AgentChargesStrBuilder.append(" , ").append(chargeKey).append(":").append(OAT);
                        }
                        agentCharges.put(chargeKey, OAT);
                    }
                }
                else
                {
                    if (chargeKey.length() < 3)
                    {
                        chargeKey = chargeKey + Constants.CARRIER_PREFIX;
                    }
                    if (!carrierCharges.containsKey(chargeKey))
                    {
                        if (CarrierChargesStrBuilder.isEmpty())
                        {
                            CarrierChargesStrBuilder.append(chargeKey).append(":").append(OAT);
                        }
                        else
                        {
                            CarrierChargesStrBuilder.append(" , ").append(chargeKey).append(":").append(OAT);
                        }
                        carrierCharges.put(chargeKey, OAT);
                    }
                }
            }
        }

        List<String> otherCharges = new ArrayList<>();
        otherCharges.add(AgentChargesStrBuilder.toString());
        otherCharges.add(CarrierChargesStrBuilder.toString());
        return otherCharges;
    }

    public static String getStringFromDict(Map<String,BigDecimal> chargeDict, V1TenantSettingsResponse v1TenantSettingsResponse)
    {
        StringBuilder chargesStr = new StringBuilder();
        for(String charge : chargeDict.keySet())
        {
            if(chargesStr.isEmpty()) {
                chargesStr.append(charge).append(":").append(IReport.DisplayFormat(chargeDict.get(charge), 2, v1TenantSettingsResponse));
            } else {
                chargesStr.append(" , ").append(charge).append(":").append(IReport.DisplayFormat(chargeDict.get(charge), 2, v1TenantSettingsResponse));
            }
        }
        return chargesStr.toString();
    }

    public static OtherChargesResponse getOtherChargesDetailsIATA(List<AwbOtherChargesInfo> otherChargesRows, Awb siData, V1TenantSettingsResponse v1TenantSettingsResponse, AwbCargoInfo cargoInfoRows)
    {
        Map<String,BigDecimal> carrierChargesIATA = new HashMap<>();
        Map<String,BigDecimal> agentChargesIATA = new HashMap<>();
        List<String> newOtherChargesList = new ArrayList<>();
        OtherChargesResponse otherChargesResponses = new OtherChargesResponse();

        for(AwbOtherChargesInfo chargeRow : emptyIfNull(otherChargesRows))
        {
            ChargesDue chargeDue = ChargesDue.getById(chargeRow.getChargeDue());
            BigDecimal chargeAmount = chargeRow.getAmount() != null ? chargeRow.getAmount() : BigDecimal.ZERO;
            String newOtherCharges = "";
            if(chargeRow.getIataDescription() != null)
            {
                chargeRow.setIataDescription(chargeRow.getIataDescription().toUpperCase());
                String chargeKey = chargeRow.getIataDescription();
                newOtherCharges += chargeKey + " : " + (siData.getAwbCargoInfo() != null ? siData.getAwbCargoInfo().getCurrency() : "") + " " + AmountNumberFormatter.Format(chargeAmount, cargoInfoRows.getCurrency(), v1TenantSettingsResponse);

                if(chargeDue == ChargesDue.AGENT) {
                    if (chargeKey.length() < 3)
                    {
                        chargeKey = chargeKey + Constants.AGENT_PREFIX;
                    }
                    if(agentChargesIATA.containsKey(chargeKey)) {
                        agentChargesIATA.put(chargeKey, agentChargesIATA.get(chargeKey).add(chargeAmount));
                    } else {
                        agentChargesIATA.put(chargeKey, chargeAmount);
                    }
                } else {
                    if (chargeKey.length() < 3)
                    {
                        chargeKey = chargeKey + Constants.CARRIER_PREFIX;
                    }
                    if (carrierChargesIATA.containsKey(chargeKey)) {
                        carrierChargesIATA.put(chargeKey, carrierChargesIATA.get(chargeKey).add(chargeAmount));
                    } else {
                        carrierChargesIATA.put(chargeKey, chargeAmount);
                    }
                }
            }
            newOtherChargesList.add(newOtherCharges);
        }

        String CarrierChargesIATAStr = getStringFromDict(carrierChargesIATA, v1TenantSettingsResponse);
        String AgentChargesIATAStr = getStringFromDict(agentChargesIATA, v1TenantSettingsResponse);
        List<String> otherChargesIATA = Arrays.asList(CarrierChargesIATAStr, AgentChargesIATAStr);

        otherChargesResponses.setOtherChargesItems(otherChargesIATA);
        otherChargesResponses.setNewOtherChargesItems(newOtherChargesList);

        return otherChargesResponses;
    }

    private String getSpecialHandlingCodes(List<AwbSpecialHandlingCodesMappingInfo> specialHandlingCodesRows)
    {
        StringBuilder specialHandlingCodesBuilder = new StringBuilder();
        for(AwbSpecialHandlingCodesMappingInfo specialHandlingCodesRow : emptyIfNull(specialHandlingCodesRows))
        {
            if(specialHandlingCodesBuilder.isEmpty())
            {
                specialHandlingCodesBuilder.append(specialHandlingCodesRow.getShcId());
            }
            else
            {
                specialHandlingCodesBuilder.append(", ").append(specialHandlingCodesRow.getShcId());
            }
        }
        return specialHandlingCodesBuilder.toString();
    }

    private List<String> getIATADescription(List<AwbOtherChargesInfo> otherChargesRows) {

        List<String> iataDescriptionList = new ArrayList<>();

        for (AwbOtherChargesInfo chargeRow : emptyIfNull(otherChargesRows)) {
            ChargesDue chargeDue = ChargesDue.getById(chargeRow.getChargeDue());
            if (chargeRow.getIataDescription() != null) {
                chargeRow.setIataDescription(chargeRow.getIataDescription().toUpperCase());
                String chargeKey = chargeRow.getIataDescription();
                String prefix = chargeDue == ChargesDue.AGENT ? Constants.AGENT_PREFIX
                    : Constants.CARRIER_PREFIX;

                if (chargeKey.length() < 3) {
                    chargeKey = chargeKey + prefix;
                    iataDescriptionList.add(chargeKey);
                }
            }
        }

        return iataDescriptionList;
    }
    private Map<String, EntityTransferMasterLists> getMasterData(Set<String> querySet) {
        MasterListRequestV2 requests = new MasterListRequestV2();
        Map<String, EntityTransferMasterLists> keyMasterDataMap = new HashMap<>();
        String[] query;
        for(String key: querySet)
        {
            query = key.split("#");
            String itemType = query[0];
            String itemValue = query[1];
            requests.getMasterListRequests().add(MasterListRequest.builder().ItemType(itemType).ItemValue(itemValue).build());
        }
        if(requests.getMasterListRequests().size() > 0) {
            V1DataResponse response = v1Service.fetchMultipleMasterData(requests);
            List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
            masterLists.forEach(masterData -> {
                String key =  MasterDataType.masterData(masterData.ItemType).getDescription()  + '#' + masterData.ItemValue;
                keyMasterDataMap.put(key, masterData);
            });
            return keyMasterDataMap;
        }
        return null;
    }

    private Map<String, UnlocationsResponse> getLocationData(Set<String> locCodes, String fieldName) {
        Map<String, UnlocationsResponse> locationMap = new HashMap<>();
        if(!List.of(EntityTransferConstants.NAME, EntityTransferConstants.LOCATION_SERVICE_GUID).contains(fieldName))
            return locationMap;
        if (locCodes.size() > 0) {
            List<Object> criteria = Arrays.asList(
                    Arrays.asList(fieldName),
                    "In",
                    Arrays.asList(locCodes)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
            V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
            if (unlocationsResponse != null && unlocationsResponse.size() > 0) {

                for (UnlocationsResponse unlocation : unlocationsResponse) {
                    switch (fieldName) {
                        case EntityTransferConstants.NAME -> locationMap.put(unlocation.getName(), unlocation);
                        case EntityTransferConstants.LOCATION_SERVICE_GUID ->
                                locationMap.put(unlocation.getLocationsReferenceGUID(), unlocation);
                        default -> {
                        }
                    }
                }
            }
        }
        return locationMap;
    }

    private Map<String, EntityTransferCarrier> fetchCarrier(Set<String> values) {
        if (values.size() == 1 && values.contains(null)) return new HashMap<>();
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ITEM_VALUE));
        String operator = Operators.IN.getValue();
        List<Object> criteria = new ArrayList<>(List.of(field, operator, List.of(values)));
        request.setCriteriaRequests(criteria);
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(request);
        V1DataResponse response = v1Service.fetchCarrierMasterData(carrierListObject, true);

        List<EntityTransferCarrier> carrierList = jsonHelper.convertValueToList(response.entities, EntityTransferCarrier.class);
        Map<String, EntityTransferCarrier> keyCarrierDataMap = new HashMap<>();
        carrierList.forEach(carrier -> {
            keyCarrierDataMap.put(carrier.getItemValue(), carrier);
        });
        return keyCarrierDataMap;
    }

    private String cityFromOrganizations (String orgName) {
        if(orgName != null){
            CommonV1ListRequest orgRequest = new CommonV1ListRequest();
            List<Object> orgField = new ArrayList<>(List.of("FullName"));
            List<Object> orgCriteria = new ArrayList<>(List.of(orgField, "=", orgName));
            orgRequest.setCriteriaRequests(orgCriteria);
            V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest);
            List<EntityTransferOrganizations> orgList = jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class);
            if(orgList != null && orgList.size() > 0) {
                return Objects.equals(null, orgList.get(0).City) ? "" : orgList.get(0).City;
            }
        }
        return "";
    }

    private String upperCase(String input) {
        return input == null ? null : input.toUpperCase();
    }
}
