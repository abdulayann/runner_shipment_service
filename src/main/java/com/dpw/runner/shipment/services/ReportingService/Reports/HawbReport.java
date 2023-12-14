package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.Models.HawbModel;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.OtherChargesResponse;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.dto.request.awb.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.enums.ChargesDue;
import com.dpw.runner.shipment.services.entity.enums.RateClass;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.fasterxml.jackson.core.type.TypeReference;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

@Component
public class HawbReport extends IReport{

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IV1Service v1Service;

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
        if(hawbModel.shipmentDetails != null && hawbModel.shipmentDetails.getConsolidationList() != null && !hawbModel.shipmentDetails.getConsolidationList().isEmpty())
        {
            hawbModel.setConsolidationDetails(hawbModel.shipmentDetails.getConsolidationList().get(0));
            hawbModel.setMawb(getMawb(hawbModel.getConsolidationDetails().getId()));
        }
        hawbModel.awb = getHawb(id);
        hawbModel.setEntityType("HAWB");
        return hawbModel;
    }

    @Override
    public Map<String, Object> populateDictionary(IDocumentModel documentModel) {

        HawbModel hawbModel = (HawbModel) documentModel;
        String json = jsonHelper.convertToJson(hawbModel.shipmentDetails);
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(json);

        //TODO- Tenant data
//        var tenantDetails = ReportHelper.getOrgAddress(siData.tenant.TenantName, siData.tenant.Address1, siData.tenant.Address2, siData.tenant.City, siData.tenant.Email, siData.tenant.Phone, siData.tenant.ZipPostCode, siData.tenant.State);
//        dictionary[ReportConstants.AGENT] = tenantDetails;

        populateUserFields(hawbModel.usersDto, dictionary);

        // Get the shipmentInforRow
        AwbShipmentInfo shipmentInfo = hawbModel.awb.getAwbShipmentInfo();


        if(shipmentInfo != null){
            List<String> shipper = getFormattedDetails(shipmentInfo.getShipperName(), shipmentInfo.getShipperAddress());
            List<String> consignee = getFormattedDetails(shipmentInfo.getConsigneeName(), shipmentInfo.getConsigneeAddress());
            dictionary.put(ReportConstants.SHIPPER_ADDRESS, shipper);
            dictionary.put(ReportConstants.CONSIGNEE_ADDRESS,  consignee);
            dictionary.put(ReportConstants.ISSUING_CARRIER_AGENT_NAME, shipmentInfo.getIssuingAgentName());
            dictionary.put(ReportConstants.ISSUiNG_CARRIER_CITY, cityFromOrganizations(shipmentInfo.getIssuingAgentName()).toUpperCase());
            dictionary.put(ReportConstants.AGENT_IATA_CODE , shipmentInfo.getIataCode().toUpperCase());
            dictionary.put(ReportConstants.CASSCODE , shipmentInfo.getAgentCASSCode().toUpperCase());
            dictionary.put(ReportConstants.FIRST_CARRIER, shipmentInfo.getFirstCarrier());

            Set<String> locCodes = new HashSet<>();

            if (shipmentInfo.getOriginAirport() != null)
            {
                locCodes.add(shipmentInfo.getOriginAirport());
            }

            if (shipmentInfo.getDestinationAirport() != null)
            {
                locCodes.add(shipmentInfo.getDestinationAirport());
            }

            Map<String, UnlocationsResponse> locCodeMap = getLocationData(locCodes);
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
                    masterDataQuery.add(MasterDataType.PAYMENT.name() + "#" + consolRow.getPayment());
                }
                //dictionary["PrintUserName"] = consolRow.InsertUserIdUsername;
            }
            else
            {
                ShipmentModel shipmentRow = hawbModel.shipmentDetails;
                dictionary.put(ReportConstants.TRANSPORT_MODE,  shipmentRow.getTransportMode());
                dictionary.put(ReportConstants.JOB_NUMBER, shipmentRow.getShipmentId());
                if(hawbModel.getEntityType().equalsIgnoreCase(AwbConstants.DMAWB))
                {
                    dictionary.put(ReportConstants.MAWB_NO, shipmentInfo.getAwbNumber());
                    AwbNumber = shipmentInfo.getAwbNumber();
                }
                else
                {
                    dictionary.put(ReportConstants.HAWB_NO, shipmentInfo.getAwbNumber());
                    AwbNumber = hawbModel.getMawb() == null || hawbModel.getMawb().getAwbShipmentInfo() == null || StringUtility.isEmpty(hawbModel.getMawb().getAwbShipmentInfo().getAwbNumber()) ? "": hawbModel.getMawb().getAwbShipmentInfo().getAwbNumber() ;
                }
                dictionary.put(ReportConstants.NEUTRAL_AWB_NO, shipmentInfo.getAwbNumber());

                if (StringUtility.isNotEmpty(shipmentRow.getPaymentTerms()))
                {
                    masterDataQuery.add(MasterDataType.PAYMENT.name() + "#" + shipmentRow.getPaymentTerms());
                }
                dictionary.put(ReportConstants.PRINT_USER_NAME, shipmentRow.getAssignedTo());
            }
            if(StringUtility.isNotEmpty(AwbNumber)){
                AwbNumber = AwbNumber.replace("-", "");
                dictionary.put(ReportConstants.MAWB_NO , AwbNumber.substring(0, Math.min(3, AwbNumber.length())));
                if(AwbNumber.length() > 3) dictionary.put(ReportConstants.MAWB_REMAINING, AwbNumber.substring(3, AwbNumber.length() - 3));
            }

            AwbCargoInfo cargoInfoRows = hawbModel.getAwb().getAwbCargoInfo();
            String NtrQtyGoods = null;
            EntityTransferMasterLists paymentCodeDetails = null;
            if(cargoInfoRows != null){
                dictionary.put(ReportConstants.REFERENCE_NUMBER, cargoInfoRows.getReferenceNumber());
                dictionary.put(ReportConstants.OPTIONAL_SHIPPING_INFORMATION, cargoInfoRows.getShippingInformation());
                dictionary.put(ReportConstants.OPTIONAL_SHIPPING_INFORMATION_OTHER, cargoInfoRows.getShippingInformationOther());
                dictionary.put(ReportConstants.OTHER_INFORMATION, cargoInfoRows.getOtherInfo());
                BigDecimal amountOfInsurance = cargoInfoRows.getInsuranceAmount();
                BigDecimal carriageValue = cargoInfoRows.getCarriageValue();
                BigDecimal customsValue = cargoInfoRows.getCustomsValue();
                BigDecimal zeroDecimal = BigDecimal.ZERO;
                if(amountOfInsurance != null && !Objects.equals(amountOfInsurance, zeroDecimal)) {
                    dictionary.put(ReportConstants.AOI, IReport.twoDecimalPlacesFormatDecimal(amountOfInsurance));
                    dictionary.put(ReportConstants.AMOUNT_OF_INSURANCE, IReport.addCommas(amountOfInsurance));
                } else {
                    dictionary.put(ReportConstants.AMOUNT_OF_INSURANCE, "XXX");
                    dictionary.put(ReportConstants.AOI , "XXX");
                }
                if(carriageValue != null && !Objects.equals(carriageValue, zeroDecimal)) {
                    dictionary.put(ReportConstants.DECLARED_VALUE_FOR_CARRIAGE,  IReport.addCommas(carriageValue));
                } else {
                    dictionary.put(ReportConstants.DECLARED_VALUE_FOR_CARRIAGE,  "NVD");
                }
                if(customsValue!= null && !Objects.equals(customsValue, zeroDecimal)) {
                    dictionary.put(ReportConstants.DECLARED_VALUE_FOR_CUSTOMS, IReport.addCommas(customsValue));
                } else {
                    dictionary.put(ReportConstants.DECLARED_VALUE_FOR_CUSTOMS, "NCV");
                }
                dictionary.put(ReportConstants.CURRENCY, cargoInfoRows.getCurrency());
                dictionary.put(ReportConstants.CHARGE_CODE, cargoInfoRows.getChargeCode());
                dictionary.put(ReportConstants.ACCOUNTING_INFORMATION,  cargoInfoRows.getAccountingInfo());
                dictionary.put(ReportConstants.HANDLING_INFORMATION, cargoInfoRows.getHandlingInfo());
                NtrQtyGoods = cargoInfoRows.getNtrQtyGoods();
                dictionary.put(ReportConstants.NATURE_OF_GOODS, NtrQtyGoods);
                dictionary.put(ReportConstants.SCI, cargoInfoRows.getSci());

                masterDataQuery.add(MasterDataType.PAYMENT_CODES.getDescription() + "#" + cargoInfoRows.getChargeCode());

            }
            List<AwbGoodsDescriptionInfo> awbGoodsDescriptionInfo = hawbModel.awb.getAwbGoodsDescriptionInfo();
            AtomicInteger TotalPieces = new AtomicInteger();
            final BigDecimal[] TotalGrossWeight = {BigDecimal.ZERO};
            final BigDecimal[] SumOfTotalAmount = {BigDecimal.ZERO};
            final BigDecimal[] SumOfChargeableWt = {BigDecimal.ZERO};
            String FreightAmountText = "";
            String OtherAmountText = "";

            masterDataQuery.add(MasterDataType.MAWB_CHARGE_TEXT.getDescription() + "#" + AwbConstants.FREIGHT_AMOUNT);
            masterDataQuery.add(MasterDataType.MAWB_CHARGE_TEXT.getDescription() + "#" + AwbConstants.OTHER_AMOUNT);

            if (originAirport != null)
            {
                masterDataQuery.add(MasterDataType.COUNTRIES.getDescription() + "#" + originAirport.getCountry());
                dictionary.put(ReportConstants.AIRPORT_OF_DEPARTURE , originAirport.getName().toUpperCase());
            }
            if (destinationAirport != null)
            {
                masterDataQuery.add(MasterDataType.COUNTRIES.getDescription() + "#" + destinationAirport.getCountry());
                dictionary.put(ReportConstants.AIRPORT_OF_DESTINATION, originAirport.getName().toUpperCase());
            }

            Map<String, EntityTransferMasterLists> dataMap = getMasterData(masterDataQuery);

            EntityTransferMasterLists MAWBChargeTextDetails = null;
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
                        paymentTerms = dataMap.get(MasterDataType.PAYMENT.name() + "#" + consolRow.getPayment());
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
                        paymentTerms = dataMap.get(MasterDataType.PAYMENT.name() + "#" + hawbModel.shipmentDetails.getPaymentTerms());
                        if (paymentTerms != null)
                        {
                            dictionary.put(ReportConstants.PAYMENT_TERMS, paymentTerms.ItemDescription);
                        }
                    }
                }

                if (originAirport != null && dataMap.get(MasterDataType.COUNTRIES.name() + "#" + originAirport.getCountry()) != null)
                {
                    dictionary.put(ReportConstants.DEPARTURE_AIRPORT_COUNTRY, dataMap.get(MasterDataType.COUNTRIES.name() + "#" + originAirport.getCountry()).ItemDescription.toUpperCase());
                }

                if (destinationAirport != null && dataMap.get(MasterDataType.COUNTRIES.name() + "#" + destinationAirport.getCountry()) != null)
                {
                    dictionary.put(ReportConstants.DESTINATION_AIRPORT_COUNTRY, dataMap.get(MasterDataType.COUNTRIES.name() + "#" + destinationAirport.getCountry()).ItemDescription.toUpperCase());
                }
            }


            dictionary.put(ReportConstants.FREIGHT_AMOUNT_TEXT,  FreightAmountText);
            dictionary.put(ReportConstants.OTHER_AMOUNT_TEXT, OtherAmountText);

            if (awbGoodsDescriptionInfo != null && awbGoodsDescriptionInfo.size() > 0){
                String finalNtrQtyGoods = NtrQtyGoods;

                List<Map<String,Object>> values = jsonHelper.convertValue(awbGoodsDescriptionInfo, new TypeReference<>(){});
                List<Map<String,Object>> valuesFAT = jsonHelper.convertValue(awbGoodsDescriptionInfo, new TypeReference<>(){});
                values.forEach(value -> {
                    value.put(ReportConstants.NATURE_QLTY_OF_GOODS, finalNtrQtyGoods);
                    if(value.get(ReportConstants.RATE_CLASS) != null){
                        value.put(ReportConstants.RATE_CLASS, RateClass.getById((Integer) value.get(ReportConstants.RATE_CLASS)));
                    }
                    if(value.get(ReportConstants.GROSS_WT) != null){
                        value.put(ReportConstants.GROSS_WT, IReport.addCommas(value.get(ReportConstants.GROSS_WT).toString()));
                    }
                    if(value.get(ReportConstants.CHARGEABLE_WT) != null){
                        value.put(ReportConstants.CHARGEABLE_WT, IReport.addCommas(value.get(ReportConstants.CHARGEABLE_WT).toString()));
                    }
                    if(value.get(ReportConstants.RATE_CHARGE) != null){
                        value.put(ReportConstants.RATE_CHARGE, IReport.addCommas(value.get(ReportConstants.RATE_CHARGE).toString()));
                    }
                    if(value.get(ReportConstants.TOTAL_AMOUNT) != null){
                        value.put(ReportConstants.TOTAL_AMOUNT, IReport.addCommas(value.get(ReportConstants.TOTAL_AMOUNT).toString()));
                    }});
                dictionary.put(ReportConstants.PACKING_LIST, values);
                String finalFreightAmountText = FreightAmountText;
                valuesFAT.forEach(value ->
                        {
                            value.put(ReportConstants.NATURE_QLTY_OF_GOODS, finalNtrQtyGoods);
                            if(value.get(ReportConstants.RATE_CLASS) != null){
                                value.put(ReportConstants.RATE_CLASS, RateClass.getById((Integer) value.get(ReportConstants.RATE_CLASS)));
                            }
                            if(value.get(ReportConstants.GROSS_WT) != null){
                                value.put(ReportConstants.GROSS_WT, IReport.addCommas(value.get(ReportConstants.GROSS_WT).toString()));
                            }
                            if(value.get(ReportConstants.CHARGEABLE_WT) != null){
                                value.put(ReportConstants.CHARGEABLE_WT, IReport.addCommas(value.get(ReportConstants.CHARGEABLE_WT).toString()));
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
                dictionary.put(ReportConstants.TOtAl_PIECES, TotalPieces);
                dictionary.put(ReportConstants.TOTAL_GROSS_WEIGHT, IReport.addCommas(TotalGrossWeight[0]));
                dictionary.put(ReportConstants.TGW, IReport.addCommas(TotalGrossWeight[0]));
                dictionary.put(ReportConstants.SUM_OF_TOTAL_AMOUNT, IReport.addCommas(SumOfTotalAmount[0]));
                dictionary.put(ReportConstants.SUM_OF_TOTAL_AMOUNT_FAT, FreightAmountText);
                dictionary.put(ReportConstants.SUM_OF_CHARGEABLE_WT, IReport.addCommas(SumOfChargeableWt[0]));
            }
            List<AwbRoutingInfo> routingInfoRows = hawbModel.awb.getAwbRoutingInfo();
            Set<String> carrierSet;
            if(routingInfoRows != null && routingInfoRows.size() > 0){
                locCodes = new HashSet<>();
                locCodes.add(routingInfoRows.get(0).getDestination());
                locCodes.add(routingInfoRows.get(0).getOrigin());
                locCodeMap = getLocationData(locCodes);
                dictionary.put(ReportConstants.TO_FIRST, locCodeMap.get(routingInfoRows.get(0).getDestination()).getIataCode());
                dictionary.put(ReportConstants.TO, dictionary.get(ReportConstants.TO_FIRST));
                dictionary.put(ReportConstants.AO_DEPT_CODE, locCodeMap.get(routingInfoRows.get(0).getOrigin()).getIataCode());
                dictionary.put(ReportConstants.ISSUED_BY, routingInfoRows.get(0).getByCarrier());
                dictionary.put(ReportConstants.FLIGHT_NO1, routingInfoRows.get(0).getFlightNumber());
                dictionary.put(ReportConstants.FLIGHT_DATE1, IReport.ConvertToDPWDateFormat(routingInfoRows.get(0).getFlightDate()));

                carrierSet = new HashSet<>();
                carrierSet.add(routingInfoRows.get(0).getByCarrier());
                Map<String, EntityTransferCarrier> carrierRow = fetchCarrier(carrierSet);
                if (carrierRow.size() > 0)
                {
                    dictionary.put(ReportConstants.BY_FIRST, carrierRow.get(routingInfoRows.get(0).getByCarrier()).IATACode);
                    dictionary.put(ReportConstants.ISSUED_BY_NAME, carrierRow.get(routingInfoRows.get(0).getByCarrier()).ItemDescription);
                }
                else
                {
                    dictionary.put(ReportConstants.BY_FIRST, "");
                }
                dictionary.put(ReportConstants.BY, dictionary.get(ReportConstants.BY_FIRST));

                List<String> flightNumberList = new ArrayList<>();
                List<String> flightDateList = new ArrayList<>();
                flightNumberList.add(String.format("{0}{1}", dictionary.get(ReportConstants.BY_FIRST), dictionary.get(ReportConstants.FLIGHT_NO1)));
                flightDateList.add(IReport.ConvertToDPWDateFormat(routingInfoRows.get(0).getFlightDate()));


                if(routingInfoRows.size()>=2){
                    locCodes = new HashSet<>();
                    locCodes.add(routingInfoRows.get(1).getDestination());
                    locCodeMap = getLocationData(locCodes);
                    dictionary.put(ReportConstants.TO_SECOND, locCodeMap.get(routingInfoRows.get(1).getDestination()).getIataCode());
                    carrierSet = new HashSet<>();
                    carrierSet.add(routingInfoRows.get(1).getByCarrier());
                    carrierRow = fetchCarrier(carrierSet);
                    if (carrierRow.size() > 0)
                    {
                        dictionary.put(ReportConstants.BY_SECOND, carrierRow.get(routingInfoRows.get(0).getByCarrier()).IATACode);
                    }
                    dictionary.put(ReportConstants.FLIGHT_NO2, routingInfoRows.get(1).getFlightNumber());
                    dictionary.put(ReportConstants.FLIGHT_DATE2, IReport.ConvertToDPWDateFormat(routingInfoRows.get(1).getFlightDate()));
                    flightNumberList.add(String.format("{0}{1}", dictionary.get(ReportConstants.BY_SECOND), dictionary.get(ReportConstants.FLIGHT_NO2)));
                    flightDateList.add(IReport.ConvertToDPWDateFormat(routingInfoRows.get(1).getFlightDate()));
                }
                if(routingInfoRows.size()>=3){
                    locCodes = new HashSet<>();
                    locCodes.add(routingInfoRows.get(2).getDestination());
                    locCodeMap = getLocationData(locCodes);
                    dictionary.put(ReportConstants.TO_THIRD, locCodeMap.get(routingInfoRows.get(2).getDestination()).getIataCode());
                    carrierSet = new HashSet<>();
                    carrierSet.add(routingInfoRows.get(2).getByCarrier());
                    carrierRow = fetchCarrier(carrierSet);
                    if (carrierRow.size() > 0)
                    {
                        dictionary.put(ReportConstants.BY_THIRD, carrierRow.get(routingInfoRows.get(0).getByCarrier()).IATACode);
                    }
                    dictionary.put(ReportConstants.FLIGHT_NO3, routingInfoRows.get(2).getFlightNumber());
                    dictionary.put(ReportConstants.FLIGHT_DATE3, IReport.ConvertToDPWDateFormat(routingInfoRows.get(2).getFlightDate()));
                    flightNumberList.add(String.format("{0}{1}", dictionary.get(ReportConstants.BY_THIRD), dictionary.get(ReportConstants.FLIGHT_NO3)));
                    flightDateList.add(IReport.ConvertToDPWDateFormat(routingInfoRows.get(2).getFlightDate()));
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
                dictionary.put(ReportConstants.TOTAL_COLLECT, IReport.addCommas(paymentInfoRows.getTotalCollect()));
                dictionary.put(ReportConstants.TOTAL_PREPAID, IReport.addCommas(paymentInfoRows.getTotalPrepaid()));
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

                    if (paymentCodeDetails.Identifier1.equalsIgnoreCase("true"))
                    {
                        dictionary.put(ReportConstants.WTVALP, "X");
                        dictionary.put(ReportConstants.WT_CHARGE_P, IReport.addCommas(paymentInfoRows.getWeightCharges()));
                        dictionary.put(ReportConstants.VALUATION_CHARGES_P, IReport.addCommas(paymentInfoRows.getValuationCharge()));
                        dictionary.put(ReportConstants.TAX_P, IReport.addCommas(paymentInfoRows.getTax()));
                        dictionary.put(ReportConstants.TOTAL_FREIGHT_P, IReport.addCommas(totalFreight));
                        dictionary.put(ReportConstants.FREIGHT_AMOUNT_TEXT_P, FreightAmountText);
                    }
                    if (paymentCodeDetails.Identifier2.equalsIgnoreCase("true"))
                    {
                        dictionary.put(ReportConstants.WTVALC, "X");
                        dictionary.put(ReportConstants.WT_CHARGE_C, IReport.addCommas(paymentInfoRows.getWeightCharges()));
                        dictionary.put(ReportConstants.VALUATION_CHARGES_C, IReport.addCommas(paymentInfoRows.getValuationCharge()));
                        dictionary.put(ReportConstants.TAX_C, IReport.addCommas(paymentInfoRows.getTax()));
                        dictionary.put(ReportConstants.TOTAL_FREIGHT_C, IReport.addCommas(totalFreight));
                        dictionary.put(ReportConstants.FREIGHT_AMOUNT_TEXT_C, FreightAmountText);
                    }
                    if (paymentCodeDetails.Identifier3.equalsIgnoreCase("true"))
                    {
                        dictionary.put(ReportConstants.OTHERS_P, "X");
                        dictionary.put(ReportConstants.AGENT_DUE_P, IReport.addCommas(paymentInfoRows.getDueAgentCharges()));
                        dictionary.put(ReportConstants.CARRIER_DUE_P, IReport.addCommas(paymentInfoRows.getDueCarrierCharges()));
                        dictionary.put(ReportConstants.TOTAL_OTHERS_P, IReport.addCommas(totalOthers));
                        dictionary.put(ReportConstants.OTHER_AMOUNT_TEXT_P, OtherAmountText);
                    }
                    if (paymentCodeDetails.Identifier4.equalsIgnoreCase("true"))
                    {
                        dictionary.put(ReportConstants.OTHERS_C, "X");
                        dictionary.put(ReportConstants.AGENT_DUE_C, IReport.addCommas(paymentInfoRows.getDueAgentCharges()));
                        dictionary.put(ReportConstants.CARRIER_DUE_C, IReport.addCommas(paymentInfoRows.getDueCarrierCharges()));
                        dictionary.put(ReportConstants.TOTAL_OTHERS_C, IReport.addCommas(totalOthers));
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
                locCodeMap = getLocationData(locCodes);
                dictionary.put(ReportConstants.EXECUTED_AT, locCodeMap.get(otherInfoRows.getExecutedAt()).getIataCode());
                dictionary.put(ReportConstants.EXECUTED_AT_NAME, locCodeMap.get(otherInfoRows.getExecutedAt()).getName());
                dictionary.put(ReportConstants.EXECUTED_ON, IReport.ConvertToDPWDateFormat(otherInfoRows.getExecutedOn()));
                dictionary.put(ReportConstants.SIGN_OF_SHIPPER, otherInfoRows.getShipper());
                dictionary.put(ReportConstants.SIGN_OF_ISSUING_CARRIER, otherInfoRows.getCarrier());
            }

            List<AwbOtherChargesInfo> otherChargesInfoRows = hawbModel.awb.getAwbOtherChargesInfo();
            dictionary.put(ReportConstants.OTHER_CHARGES, getOtherChargesDetails(otherChargesInfoRows, hawbModel.awb).getOtherChargesItems());
            dictionary.put(ReportConstants.NEW_OTHER_CHARGES, getOtherChargesDetails(otherChargesInfoRows,hawbModel.awb).getNewOtherChargesItems());
            dictionary.put(ReportConstants.OTHER_CHARGES_IATA, getOtherChargesDetailsIATA(otherChargesInfoRows, hawbModel.awb).getOtherChargesItems());
            dictionary.put(ReportConstants.NEW_OTHER_CHARGES_IATA, getOtherChargesDetailsIATA(otherChargesInfoRows, hawbModel.awb).getNewOtherChargesItems());
            dictionary.put(ReportConstants.OTHER_CHARGES_OAT, getOtherChargesDetailsOAT(otherChargesInfoRows,OtherAmountText));
            dictionary.put(ReportConstants.OTHER_CHARGES_IATA_OAT, getOtherChargesDetailsIATAOAT(otherChargesInfoRows, OtherAmountText));
            List<AwbSpecialHandlingCodesMappingInfo> specialHandlingCodesRows = hawbModel.awb.getAwbSpecialHandlingCodesMappings();
            dictionary.put(ReportConstants.SPECIAL_HANDLING_CODE, getSpecialHandlingCodes(specialHandlingCodesRows));
        }
        return dictionary;
    }

    public static OtherChargesResponse getOtherChargesDetails(List<AwbOtherChargesInfo> otherChargesRows, Awb siData)
    {
        OtherChargesResponse otherChargesResponses = new OtherChargesResponse();
        Map<String,BigDecimal> carrierCharges = new HashMap<>();
        Map<String,BigDecimal> agentCharges = new HashMap<>();
        List<String> newOtherChargesList = new ArrayList<>();

        for (AwbOtherChargesInfo chargeRow : otherChargesRows)
        {
            ChargesDue chargeDue = ChargesDue.getById(chargeRow.getChargeDue());
            String chargeKey = chargeRow.getChargeTypeId();
            BigDecimal chargeAmount = (chargeRow.getAmount() != null ? chargeRow.getAmount() : BigDecimal.ZERO);
            String newOtherCharges = chargeKey + " : " + (siData.getAwbCargoInfo() != null ? siData.getAwbCargoInfo().getCurrency() : "") + " " + IReport.addCommas(chargeAmount);
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

        String CarrierChargesStr = getStringFromDict(carrierCharges);
        String AgentChargesStr = getStringFromDict(agentCharges);

        List<String> otherCharges = Arrays.asList(AgentChargesStr, CarrierChargesStr);
        otherChargesResponses.setOtherChargesItems(otherCharges);
        otherChargesResponses.setOtherChargesItems(newOtherChargesList);

        return otherChargesResponses;
    }
    public static List<String> getOtherChargesDetailsOAT(List<AwbOtherChargesInfo> otherChargesRows, String OAT)
    {
        Map<String, String> carrierCharges = new HashMap<>();
        Map<String, String> agentCharges = new HashMap<>();
        String AgentChargesStr = "";
        String CarrierChargesStr = "";
        for (AwbOtherChargesInfo chargeRow : otherChargesRows)
        {
            ChargesDue chargeDue = ChargesDue.getById(chargeRow.getChargeDue());
            String chargeKey = chargeRow.getChargeTypeId();
            if (chargeDue == ChargesDue.AGENT)
            {
                if (!agentCharges.containsKey(chargeKey))
                {
                    if (StringUtility.isEmpty(AgentChargesStr))
                    {
                        AgentChargesStr = chargeKey + ":" + OAT;
                    }
                    else
                    {
                        AgentChargesStr = AgentChargesStr + " , " + chargeKey + ":" + OAT;
                    }
                    agentCharges.put(chargeKey, OAT);
                }
            }
            else
            {
                if (!carrierCharges.containsKey(chargeKey))
                {
                    if (StringUtility.isEmpty(CarrierChargesStr))
                    {
                        CarrierChargesStr = chargeKey + ":" + OAT;
                    }
                    else
                    {
                        CarrierChargesStr = CarrierChargesStr + " , " + chargeKey + ":" + OAT;
                    }
                    carrierCharges.put(chargeKey, OAT);
                }
            }

        }

        List<String> otherCharges = new ArrayList<>();
        otherCharges.add(AgentChargesStr);
        otherCharges.add(CarrierChargesStr);
        return otherCharges;
    }

    public static List<String> getOtherChargesDetailsIATAOAT(List<AwbOtherChargesInfo> otherChargesRows, String OAT)
    {
        Map<String, String> carrierCharges = new HashMap<>();
        Map<String, String> agentCharges = new HashMap<>();
        String AgentChargesStr = "";
        String CarrierChargesStr = "";
        for (AwbOtherChargesInfo chargeRow : otherChargesRows)
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
                        if (StringUtility.isEmpty(AgentChargesStr))
                        {
                            AgentChargesStr = chargeKey + ":" + OAT;
                        }
                        else
                        {
                            AgentChargesStr = AgentChargesStr + " , " + chargeKey + ":" + OAT;
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
                        if (StringUtility.isEmpty(CarrierChargesStr))
                        {
                            CarrierChargesStr = chargeKey + ":" + OAT;
                        }
                        else
                        {
                            CarrierChargesStr = CarrierChargesStr + " , " + chargeKey + ":" + OAT;
                        }
                        carrierCharges.put(chargeKey, OAT);
                    }
                }
            }
        }

        List<String> otherCharges = new ArrayList<>();
        otherCharges.add(AgentChargesStr);
        otherCharges.add(CarrierChargesStr);
        return otherCharges;
    }

    public static String getStringFromDict(Map<String,BigDecimal> chargeDict)
    {
        String chargesStr = "";
        for(String charge : chargeDict.keySet())
        {
            if(StringUtility.isEmpty(chargesStr)) {
                chargesStr = charge + ":" + IReport.addCommas(chargeDict.get(charge));
            } else {
                chargesStr = chargesStr + " , " + charge + ":" + IReport.addCommas(chargeDict.get(charge));
            }
        }
        return chargesStr;
    }

    public static OtherChargesResponse getOtherChargesDetailsIATA(List<AwbOtherChargesInfo> otherChargesRows, Awb siData)
    {
        Map<String,BigDecimal> carrierChargesIATA = new HashMap<>();
        Map<String,BigDecimal> agentChargesIATA = new HashMap<>();
        List<String> newOtherChargesList = new ArrayList<>();
        OtherChargesResponse otherChargesResponses = new OtherChargesResponse();

        for(AwbOtherChargesInfo chargeRow : otherChargesRows)
        {
            ChargesDue chargeDue = ChargesDue.getById(chargeRow.getChargeDue());
            BigDecimal chargeAmount = chargeRow.getAmount() != null ? chargeRow.getAmount() : BigDecimal.ZERO;
            String newOtherCharges = "";
            if(chargeRow.getIataDescription() != null)
            {
                chargeRow.setIataDescription(chargeRow.getIataDescription().toUpperCase());
                String chargeKey = chargeRow.getIataDescription();
                newOtherCharges += chargeKey + " : " + (siData.getAwbCargoInfo() != null ? siData.getAwbCargoInfo().getCurrency() : "") + " " + IReport.addCommas(chargeAmount);

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

        String CarrierChargesIATAStr = getStringFromDict(carrierChargesIATA);
        String AgentChargesIATAStr = getStringFromDict(agentChargesIATA);
        List<String> otherChargesIATA = Arrays.asList(CarrierChargesIATAStr, AgentChargesIATAStr);

        otherChargesResponses.setOtherChargesItems(otherChargesIATA);
        otherChargesResponses.setNewOtherChargesItems(newOtherChargesList);

        return otherChargesResponses;
    }

    private String getSpecialHandlingCodes(List<AwbSpecialHandlingCodesMappingInfo> specialHandlingCodesRows)
    {
        String specialHandlingCodes = "";
        for(AwbSpecialHandlingCodesMappingInfo specialHandlingCodesRow : specialHandlingCodesRows)
        {
            if(specialHandlingCodes.isEmpty())
            {
                specialHandlingCodes = specialHandlingCodesRow.getShcId();
            }
            else
            {
                specialHandlingCodes = specialHandlingCodes + ", " + specialHandlingCodesRow.getShcId();
            }
        }
        return specialHandlingCodes;
    }

    private Map<String, EntityTransferMasterLists> getMasterData(Set<String> querySet) {
        List<MasterListRequest> requests = new ArrayList<>();
        Map<String, EntityTransferMasterLists> keyMasterDataMap = new HashMap<>();
        String[] query;
        for(String key: querySet)
        {
            query = key.split("#");
            String itemType = query[0];
            String itemValue = query[1];
            requests.add(MasterListRequest.builder().ItemType(itemType).ItemValue(itemValue).build());
        }
        if(requests.size() > 0) {
            V1DataResponse response = v1Service.fetchMultipleMasterData(requests);
            List<EntityTransferMasterLists> masterLists = jsonHelper.convertValueToList(response.entities, EntityTransferMasterLists.class);
            masterLists.forEach(masterData -> {
                String key =  MasterDataType.masterData(masterData.ItemType).name()  + '#' + masterData.ItemValue;
                keyMasterDataMap.put(key, masterData);
            });
            return keyMasterDataMap;
        }
        return null;
    }

    private Map<String, UnlocationsResponse> getLocationData(Set<String> locCodes) {
        Map<String, UnlocationsResponse> locationMap = new HashMap<>();
        if (locCodes.size() > 0) {
            List<Object> criteria = Arrays.asList(
                    Arrays.asList(EntityTransferConstants.LOCATION_SERVICE_GUID),
                    "In",
                    Arrays.asList(locCodes)
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).take(0).criteriaRequests(criteria).build();
            V1DataResponse v1DataResponse = v1Service.fetchUnlocation(commonV1ListRequest);
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(v1DataResponse.entities, UnlocationsResponse.class);
            if (unlocationsResponse != null && unlocationsResponse.size() > 0) {

                for (UnlocationsResponse unlocation : unlocationsResponse) {
                    locationMap.put(unlocation.getLocCode(), unlocation);
                }

            }
        }
        return locationMap;
    }

    private Map<String, EntityTransferCarrier> fetchCarrier(Set<String> values) {
        CommonV1ListRequest request = new CommonV1ListRequest();
        List<Object> criteria = new ArrayList<>();
        List<Object> field = new ArrayList<>(List.of(EntityTransferConstants.ITEM_VALUE));
        String operator = Operators.IN.getValue();
        criteria.addAll(List.of(field, operator, List.of(values)));
        request.setCriteriaRequests(criteria);
        V1DataResponse response = v1Service.fetchCarrierMasterData(request, true);

        List<EntityTransferCarrier> carrierList = jsonHelper.convertValueToList(response.entities, EntityTransferCarrier.class);
        Map<String, EntityTransferCarrier> keyCarrierDataMap = new HashMap<>();
        carrierList.forEach(carrier -> {
            keyCarrierDataMap.put(carrier.getItemValue(), carrier);
        });
        return keyCarrierDataMap;
    }

    private String cityFromOrganizations (String orgName) {
        CommonV1ListRequest orgRequest = new CommonV1ListRequest();
        List<Object> orgField = new ArrayList<>(List.of("FullName"));
        String operator = Operators.IN.getValue();
        List<Object> orgCriteria = new ArrayList<>(List.of(orgField, operator, List.of(orgName)));
        orgRequest.setCriteriaRequests(orgCriteria);
        V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest);
        List<EntityTransferOrganizations> orgList = jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class);
        if(orgList != null && orgList.size() > 0) {
            return orgList.get(0).City;
        }
        return "";
    }
}
