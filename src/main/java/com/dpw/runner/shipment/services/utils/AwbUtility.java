package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.dto.request.awb.AwbAddressParam;
import com.dpw.runner.shipment.services.dto.request.awb.AwbPackingInfo;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@SuppressWarnings("rawtypes")
public class AwbUtility {

    public static String getFormattedAddress(AwbAddressParam addressParam)
    {
        String forMattedAddress = "";
        String newLine = "\r\n";
        forMattedAddress = addressParam.getAddress1();
        if (!StringUtility.isEmpty(addressParam.getAddress2()))
            forMattedAddress += newLine + addressParam.getAddress2();
        if (!StringUtility.isEmpty(addressParam.getState()))
            forMattedAddress += newLine + addressParam.getState();
        if (!StringUtility.isEmpty(addressParam.getCity()))
            forMattedAddress += newLine + addressParam.getCity();
        if (!StringUtility.isEmpty(addressParam.getCountry()))
            forMattedAddress += newLine + addressParam.getCountry();
        if (!StringUtility.isEmpty(addressParam.getPinCode()))
            forMattedAddress += newLine + addressParam.getPinCode();
        if (!StringUtility.isEmpty(addressParam.getContactNumber()))
            forMattedAddress += newLine + addressParam.getContactNumber();
        return forMattedAddress;
    }

    public static String constructAddress(Map<String, Object> addressData) {
        StringBuilder sb = new StringBuilder();
        String newLine = "\r\n";

        if(addressData != null) {
            if (addressData.containsKey(PartiesConstants.ADDRESS1))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.ADDRESS1)));
            if (addressData.containsKey(PartiesConstants.ADDRESS2))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.ADDRESS2)));
            if (addressData.containsKey(PartiesConstants.STATE))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.STATE)));
            if (addressData.containsKey(PartiesConstants.CITY))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.CITY)));
            if (addressData.containsKey(PartiesConstants.COUNTRY))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.COUNTRY)));
            if (addressData.containsKey(PartiesConstants.ZIP_POST_CODE))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.ZIP_POST_CODE)));
            if (addressData.containsKey(PartiesConstants.CONTACT_PHONE))
                sb.append(newLine).append(StringUtility.convertToString(addressData.get(PartiesConstants.CONTACT_PHONE)));
        }

        return sb.toString();
    }

    public static String generateNatureAndQuantGoodsField(String goodsDescription, BigDecimal volumeWeight, List<AwbPackingInfo> packingList) {
        String natureAndQuantGoodsValue = goodsDescription;
        String packsDescriptionValue = "";
        String dimensionText = "DIMS: In ";
        HashSet<String> uniqueDimension = new HashSet<String>();
        String newLine = "\r\n";
        char[] toTrim = {',', ' '};
        String volumetricWeight = String.format("{0:0.0#}", volumeWeight);
        String dimnAndPacksText = "";

        if (!StringUtility.isEmpty(natureAndQuantGoodsValue)) {
            natureAndQuantGoodsValue += newLine;
        }
        if (packingList != null && packingList.size() > 0) {
            var counter = 0;
            for (var packing : packingList) {
                String pieces = " ";
                String len = " ";
                String width = " ";
                String height = " ";
                String equals = "=";
                String cross = "X";

                if (!StringUtility.isEmpty(packing.getPacks())) {
                    pieces = packing.getPacks() + equals;
                } else {
                    pieces += equals;
                }

                if (packing.getLength() != null) {
                    len = packing.getLength() + cross;
                } else {
                    len += cross;
                }

                if (packing.getWidth() != null) {
                    width = packing.getWidth() + cross;
                } else {
                    width += cross;
                }

                if (packing.getHeight() != null) {
                    height = packing.getHeight().toString();
                }
                if (!StringUtility.isEmpty(packing.getLengthUnit()) &&
                        !StringUtility.isEmpty(packing.getWidthUnit()) &&
                        !StringUtility.isEmpty(packing.getHeightUnit())) {
                    uniqueDimension.add(packing.getLengthUnit());
                    uniqueDimension.add(packing.getWidthUnit());
                    uniqueDimension.add(packing.getHeightUnit());
                }
                counter++;

                packsDescriptionValue += pieces + len + width + height + ",";
                if (counter == packingList.size()) {
                    packsDescriptionValue = StringUtils.stripEnd(packsDescriptionValue, ",");
                    packsDescriptionValue = StringUtils.stripEnd(packsDescriptionValue, " ");
                }

                if (counter % 2 == 0) {
                    packsDescriptionValue += newLine;
                }
            }


            if (uniqueDimension.size() == 1) {
                String dimentionUnit = uniqueDimension.stream().findFirst().get();
                if (dimentionUnit == "CM") {
                    dimentionUnit = "CMS";
                } else if (dimentionUnit == "IN") {
                    dimentionUnit = "Inches";
                } else if (dimentionUnit == "M") {
                    dimentionUnit = "Meter";
                } else if (dimentionUnit == "FT") {
                    dimentionUnit = "Feet";
                } else {
                    dimentionUnit = "";
                }

                dimensionText += dimentionUnit + newLine;
            } else {
                dimensionText += newLine;
            }

            if (counter % 2 != 0) {
                packsDescriptionValue += newLine;
            }
            packsDescriptionValue += "Total Volumetric Weight ";

//            if (tenantSettings != null && tenantSettings.WeightChargeableUnit == "KG") { //TODO fetch values from tenant
//                packsDescriptionValue += volumetricWeight + " " + "KGS";
//            }

            dimnAndPacksText = dimensionText + packsDescriptionValue;

        }
        return natureAndQuantGoodsValue + dimnAndPacksText;
    }

    public static BigDecimal roundOffAirShipment(double charge) {
        if ((charge - 0.50) <= Math.floor(charge) && charge != Math.floor(charge))
            charge = Math.floor(charge) + 0.50;
        else
            charge = Math.ceil(charge);
        return new BigDecimal(charge);
    }

    public static void validateShipmentInfoBeforeGeneratingAwb(ShipmentDetails shipmentDetails) {
        if (shipmentDetails.getConsigner() == null || shipmentDetails.getConsigner().getId() == null) {
            throw new ValidationException("Consigner details are required in shipment to generate the document.");
        }
        if (shipmentDetails.getConsignee() == null || shipmentDetails.getConsignee().getId() == null) {
            throw new ValidationException("Consignee details are required in shipment to generate the document.");
        }
        if (shipmentDetails.getCarrierDetails().getOriginPort() == null) {
            throw new ValidationException("Port Of Loading is required in shipment to generate the document.");
        }
        if ( shipmentDetails.getCarrierDetails().getDestinationPort() == null) {
            throw new ValidationException("Port Of Destination is required in shipment to generate the document.");
        }
        if (shipmentDetails.getCarrierDetails().getId() == null) {
            throw new ValidationException("Carrier is required in shipment to generate the document.");
        }
        if (Objects.equals(shipmentDetails.getShipmentType(), ShipmentConstants.SHIPMENT_TYPE_DRT) && shipmentDetails.getMasterBill() == null) {
            throw new ValidationException("MAWB Number is required in shipment to generate the document.");
        }
        if (shipmentDetails.getHouseBill() == null) {
            throw new ValidationException("HAWB Number is required in shipment to generate the document.");
        }
    }

    public static void validateConsolidationInfoBeforeGeneratingAwb(ConsolidationDetails consolidationDetails)
    {
        if (consolidationDetails.getSendingAgent() == null)
        {
            throw new ValidationException("Sending Agent details are required in Consolidation to generate the document.");
        }
        if (consolidationDetails.getReceivingAgent() == null)
        {
            throw new ValidationException("Receiving Agent details are required in Consolidation to generate the document.");
        }
        if (consolidationDetails.getCarrierDetails() == null)
        {
            throw new ValidationException("Carrier is required in Consolidation to generate the document.");
        }
        if (consolidationDetails.getCarrierDetails().getOriginPort() == null)
        {
            throw new ValidationException("Loading Port is required in Consolidation to generate the document.");
        }
        if (consolidationDetails.getCarrierDetails().getDestinationPort() == null)
        {
            throw new ValidationException("Discharge Port is required in Consolidation to generate the document.");
        }
    }

    public static String generateNatureAndQuantFieldsForConsolMawb(String goodsDescription, BigDecimal volumeWeight, List<AwbPackingInfo> packingList)
    {
        String defaultTextForQuantAndGoods = Constants.DEFAULT_NATURE_AND_QUANTITY_GOODS_TEXT_MAWB;
        String newLine = "\r\n";
        return defaultTextForQuantAndGoods + newLine + generateNatureAndQuantGoodsField(goodsDescription, volumeWeight, packingList);
    }
}
