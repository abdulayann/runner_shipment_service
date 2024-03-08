package com.dpw.runner.shipment.services.ReportingService.Reports;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.PackingListModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.GetNextNumberHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.*;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.*;
import static com.dpw.runner.shipment.services.utils.CommonUtils.stringValueOf;

@Component
public class PackingListReport extends IReport {

    @Autowired
    JsonHelper jsonHelper;

    @Autowired
    GetNextNumberHelper getNextNumberHelper;

    @Override
    public Map<String, Object> getData(Long id) {
        PackingListModel model = (PackingListModel) getDocumentModel(id);
        return populateDictionary(model);
    }

    @Override
    IDocumentModel getDocumentModel(Long id) {
        PackingListModel packingListModel = new PackingListModel();
        var shipment = getShipment(id);
        packingListModel.setShipmentDetails(shipment);
        if(shipment.getConsolidationList() != null && shipment.getConsolidationList().size() > 0) {
            packingListModel.setConsolidation(getConsolidation(shipment.getConsolidationList().get(0).getId()));
        }
        packingListModel.setTenant(getTenant());
        packingListModel.setNoOfPackagesWord(numberToWords(shipment.getNoOfPacks()));
        packingListModel.setUserDisplayName(UserContext.getUser().DisplayName);
        packingListModel.setUser(UserContext.getUser());
        return packingListModel;
    }

    @Override
    Map<String, Object> populateDictionary(IDocumentModel documentModel) {
        PackingListModel model = (PackingListModel) documentModel;
        V1TenantSettingsResponse v1TenantSettingsResponse = TenantSettingsDetailsContext.getCurrentTenantSettings();
        var shipment = model.getShipmentDetails();
        Map<String, Object> dictionary = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(shipment));

        JsonDateFormat(dictionary);
        populateTenantFields(dictionary, model.getTenant());
        populateShipmentFields(shipment, true, dictionary);

        List<String> consigner = null;
        List<String> consignee = null;
        PartiesModel shipmentConsigner = shipment.getConsigner();
        PartiesModel shipmentConsignee = shipment.getConsignee();

        if (shipmentConsigner != null) {
            Map<String, Object> consignerAddress = shipmentConsigner.getAddressData();
            if (consignerAddress != null) {
                consigner = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consignerAddress, COMPANY_NAME), getValueFromMap(consignerAddress, ADDRESS1),
                        getValueFromMap(consignerAddress, ADDRESS2), ReportHelper.getCityCountry(getValueFromMap(consignerAddress, CITY), getValueFromMap(consignerAddress, COUNTRY)),
                        getValueFromMap(consignerAddress, EMAIL), getValueFromMap(consignerAddress, CONTACT_PHONE),
                        getValueFromMap(consignerAddress, "Zip_PostCode"));
                dictionary.put(ReportConstants.CONSIGNER_NAME, consignerAddress.get(COMPANY_NAME));
                dictionary.put(ReportConstants.CONSIGNER_CONTACT_PERSON, consignerAddress.get(CONTACT_PERSON));
            }
        }
        if (shipmentConsignee != null) {
            Map<String, Object> consigneeAddress = shipmentConsignee.getAddressData();
            if (consigneeAddress != null) {
                consignee = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consigneeAddress, COMPANY_NAME), getValueFromMap(consigneeAddress, ADDRESS1),
                        getValueFromMap(consigneeAddress, ADDRESS2),
                        ReportHelper.getCityCountry(getValueFromMap(consigneeAddress, CITY), getValueFromMap(consigneeAddress, COUNTRY)),
                        getValueFromMap(consigneeAddress, EMAIL), getValueFromMap(consigneeAddress, CONTACT_PHONE),
                        getValueFromMap(consigneeAddress, "Zip_PostCode"));
                dictionary.put(ReportConstants.CONSIGNEE_NAME, getValueFromMap(consigneeAddress, COMPANY_NAME));
                dictionary.put(ReportConstants.CONSIGNEE_CONTACT_PERSON, getValueFromMap(consigneeAddress, CONTACT_PERSON));
            }
        }

        if(shipment.getWeight() != null) {
            dictionary.put(ReportConstants.WEIGHT, ConvertToWeightNumberFormat(shipment.getWeight()));
        }
        if(shipment.getVolume() != null) {
            dictionary.put(ReportConstants.VOLUME, ConvertToVolumeNumberFormat(shipment.getVolume()));
        }
        if(shipment.getChargable() != null) {
            dictionary.put(ReportConstants.CHARGEABLE, ConvertToWeightNumberFormat(shipment.getChargable()));
        }

        dictionary.put(ReportConstants.CONSIGNER, consigner);
        dictionary.put(ReportConstants.CONSIGNEE, consignee);

        if (shipment.getConsignee() != null && shipment.getConsignee().getIsAddressFreeText() != null) {
            dictionary.put(ReportConstants.CONSIGNEE_FREETEXT, ReportHelper.getAddressList(stringValueOf(shipment.getConsignee().getAddressData().get(PartiesConstants.RAW_DATA))));
        } else {
            dictionary.put(ReportConstants.CONSIGNEE_FREETEXT, consignee);
        }

        if (shipment.getConsigner() != null && shipment.getConsigner().getIsAddressFreeText() != null) {
            dictionary.put(ReportConstants.CONSIGNER_FREETEXT, ReportHelper.getAddressList(stringValueOf(shipment.getConsigner().getAddressData().get(PartiesConstants.RAW_DATA))));
        } else {
            dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
        }

        dictionary.put(ReportConstants.CONSIGNER_ADDRESS,
                ReportHelper.getAddressList(shipment.getConsigner() != null && shipment.getConsigner().getAddressData() != null ?
                        stringValueOf(shipment.getConsigner().getAddressData().get(ReportConstants.ADDRESS1)) : null));

        dictionary.put(ReportConstants.CONSIGNEE_ADDRESS,
                ReportHelper.getAddressList(shipment.getConsignee() != null && shipment.getConsignee().getAddressData() != null ?
                        stringValueOf(shipment.getConsignee().getAddressData().get(ReportConstants.ADDRESS1)) : null));

        dictionary.put(ReportConstants.VESSEL_NAME, model.vesselName);
        dictionary.put(ReportConstants.LOGO, getPrintLogoPath(model.getUser()));

        if (shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getEtd() != null) {
            dictionary.put(ReportConstants.ETD, ConvertToDPWDateFormat(shipment.getCarrierDetails().getEtd()));
        } else {
            dictionary.put(ReportConstants.ETD, null);
        }

        if (shipment.getCarrierDetails() != null && shipment.getCarrierDetails().getEta() != null) {
            dictionary.put(ReportConstants.ETA, ConvertToDPWDateFormat(shipment.getCarrierDetails().getEta()));
        } else {
            dictionary.put(ReportConstants.ETA, null);
        }

        dictionary.put(ReportConstants.PAYMENT_TERMS, shipment.getPaymentTerms());
        dictionary.put(ReportConstants.PURCHASE_ORDER_NUMBER, shipment.getBookingNumber());
        dictionary.put(ReportConstants.PACKAGE_TYPE, shipment.getPacksUnit());

        List<ReferenceNumbersModel> listOfReferenceNo = shipment.getReferenceNumbersList();
        boolean flag = false;

        for (var item : listOfReferenceNo) {
            if (item != null && INVNO.equals(item.getType())) {
                dictionary.put(ReportConstants.INVOICE_NUMBER, item.getReferenceNumber());
                flag = true;
                break;
            }
        }

        if (!flag) {
            dictionary.put(ReportConstants.INVOICE_NUMBER, null);
        }
//        Mapping unclear
//        dictionary.put(ReportConstants.EXPORTER_TAX_ID, shipment.getConsigner().ConsignerVatRegNumber);
//        dictionary.put(ReportConstants.CONSIGNEE_TAX_ID, shipment.getConsignee().ConsignerVatRegNumber);

        dictionary.put(ReportConstants.AIRWAY_BILL_NUMBER, shipment.getHouseBill());
        dictionary.put(ReportConstants.SPECIAL_INSTRUCTION, shipment.getAdditionalTerms());

        dictionary.put(ReportConstants.SHIP_DATE, ConvertToDPWDateFormat(shipment.getShipmentCreatedOn()));

        long totalPacks = 0L;
        BigDecimal totalNetWeight = BigDecimal.ZERO;
        String unitOfTotalNetWeight = null;
        boolean breakFlagNetWeight = false;

        if(shipment.getPackingList() != null && shipment.getPackingList().size() > 0) {
            List<Map<String, Object>> values = new ArrayList<>();
            shipment.getPackingList().forEach(i ->
                values.add(jsonHelper.convertJsonToMap(jsonHelper.convertToJson(i)))
            );

            for(var v : values) {
                JsonDateFormat(v);
                totalPacks += Long.parseLong(stringValueOf(v.get("Packs")));

                if (!breakFlagNetWeight && v.containsKey(ReportConstants.NET_WEIGHT) && v.get(ReportConstants.NET_WEIGHT) != null
                        && v.containsKey(ReportConstants.NET_WEIGHT_UNIT) && v.get(ReportConstants.NET_WEIGHT_UNIT) != null) {
                    if (unitOfTotalNetWeight == null) {
                        unitOfTotalNetWeight = stringValueOf(v.get(ReportConstants.NET_WEIGHT));
                        totalNetWeight = totalNetWeight.add(new BigDecimal(stringValueOf(v.get(ReportConstants.NET_WEIGHT))));
                    } else if (!unitOfTotalNetWeight.equals(stringValueOf(v.get(ReportConstants.NET_WEIGHT)))) {
                        totalNetWeight = BigDecimal.ZERO;
                        breakFlagNetWeight = true;
                    } else {
                        totalNetWeight = totalNetWeight.add(new BigDecimal(stringValueOf(v.get(ReportConstants.NET_WEIGHT))));
                    }
                }

                if (v.containsKey(ReportConstants.NET_WEIGHT) && v.get(ReportConstants.NET_WEIGHT) != null) {
                    v.put(ReportConstants.NET_WEIGHT, ConvertToWeightNumberFormat(
                                    new BigDecimal(ReportHelper.twoDecimalPlacesFormat(stringValueOf(v.get(ReportConstants.NET_WEIGHT))))));
                }

                v.put(ReportConstants.WEIGHT, ConvertToWeightNumberFormat(new BigDecimal(stringValueOf(v.get(ReportConstants.WEIGHT)))));
                v.put(ReportConstants.PACKS, ReportHelper.addCommaWithoutDecimal(new BigDecimal(stringValueOf(v.get(PACKS)))));

                if(v.get(ReportConstants.SHIPMENT_PACKING_PACKS_PACKSTYPE) != null) {
                    var packsMasterData = getMasterListData(MasterDataType.PACKS_UNIT, stringValueOf(v.get(ReportConstants.SHIPMENT_PACKING_PACKS_PACKSTYPE)).toUpperCase());
                    v.put(ReportConstants.SHIPMENT_PACKING_PACKS_PACKSTYPEDESCRIPTION, packsMasterData != null ? packsMasterData.getItemDescription() : null);
                }

            }

            dictionary.put(ReportConstants.SHIPMENT_PACKING_ITEMS, values);
        }

        if (totalPacks != 0) {
            dictionary.put(ReportConstants.TOTAL_PACKS, AmountNumberFormatter.Format(BigDecimal.valueOf(totalPacks), shipment.getFreightLocalCurrency(), v1TenantSettingsResponse));
        } else {
            dictionary.put(ReportConstants.TOTAL_PACKS, null);
        }

        if (breakFlagNetWeight || totalNetWeight.equals(BigDecimal.ZERO)) {
            dictionary.put(ReportConstants.SHIPMENT_PACKING_TOTALNETWEIGHT, null);
            dictionary.put(ReportConstants.SHIPMENT_PACKING_PACKS_UOTNW, null);
        } else {
            dictionary.put(ReportConstants.SHIPMENT_PACKING_TOTALNETWEIGHT,
                    ReportHelper.twoDecimalPlacesFormat(totalNetWeight.toString()));
            dictionary.put(ReportConstants.SHIPMENT_PACKING_PACKS_UOTNW, unitOfTotalNetWeight);
        }

        if(shipment.getPaymentTerms() != null) {
            var packsMasterData = getMasterListData(MasterDataType.PAYMENT, shipment.getPaymentTerms());
            dictionary.put(ReportConstants.PACKS_UNIT_DESC, packsMasterData != null ? packsMasterData.getItemDescription() : null);
        }

        if(shipment.getPacksUnit() != null) {
            var packsMasterData = getMasterListData(MasterDataType.PACKS_UNIT, shipment.getPacksUnit());
            dictionary.put(ReportConstants.PACKS_UNIT_DESC, packsMasterData != null ? packsMasterData.getItemDescription() : null);
        }

        if(shipment.getAdditionalDetails() != null && shipment.getAdditionalDetails().getGoodsCO() != null) {
            var masterData = getMasterListData(MasterDataType.COUNTRIES, shipment.getAdditionalDetails().getGoodsCO());
            dictionary.put(ReportConstants.COUNTRY_OF_GOODS_ORIGIN, masterData != null ? masterData.getItemDescription() : null);
        }

        return dictionary;
    }

    private String getLogoPath(UsersDto user) {
        String basePath = "Upload/";
        return basePath + user.TenantId + "/Assets/" + user.TenantPrintLogo;

    }

    private String getPrintLogoPath(UsersDto user) {
        var pathbase = "Upload/";
        var path = pathbase + user.TenantId + "/Assets/" + user.TenantPrintLogo;
        if (user.TenantPrintLogo != null) {
            return path;
        }
        return null;
    }
}
