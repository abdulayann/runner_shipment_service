package com.dpw.runner.shipment.services.ReportingService.Reports;

import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ADDITIONAL_COST_AT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ADDITIONAL_SECURITY_INFORMATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ADDRESS1;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ADDRESS2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIR;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIRLINE_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIR_DG_CLASS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIR_DG_CLASS_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AIR_UN_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AMS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AMS_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AOM_FREE_TEXT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.AS_AGREED;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ATA_OR_ETA;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BL_CHARGES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BL_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BL_PLACE_OF_DELIVERY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BL_PLACE_OF_RECEIPT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BL_PORT_OF_DISCHARGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BL_PORT_OF_LOADING;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BL_IS_NOT_RATED;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BL_IS_NOT_RATED_VALUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BOOKING_ORDER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BOUNDED_WAREHOUSE_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.BOUNDED_WAREHOUSE_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CAL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CARGO_GROSS_VOLUME_UNIT_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CARGO_GROSS_WEIGHT_UNIT_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CARGO_LOCATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CARGO_TERMS_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CARRIER_CONTACT_PERSON;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CARRIER_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CARRIER_SEAL_NUMBER2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CEN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGABLE_AND_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGEABLE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGEABLE_AND_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGEABLE_UNIT1;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGEABLE_WEIGHT_DECIMAL_PLACES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGEABLE_WT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGES_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGES_SMALL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CHARGE_TYPE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CITY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CLASS_DIVISION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CLIENT_ADDRESS_CITY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CLIENT_ADDRESS_COUNTRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CLIENT_ADDRESS_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CLIENT_ADDWO_CONT_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CLIENT_ADD_WITHOUT_CONTACT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CLIENT_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_CONSIGNEE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_CONSIGNEE_AWB_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_CONSIGNER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_DESTINATION_AGENT_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_NOTIFY_AWB_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_ORIGIN_AGENT_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CM_SHIPPER_AWB_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COMMODITY_DESC;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COMMODITY_DESC_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COMPANY_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_ADDRESS_FREE_TEXT_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_ADDRESS_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_ADDWO_CONT_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_ADD_WITHOUT_CONTACT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_COMPANY_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_FREETEXT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNEE_NAME_FREETEST_INCAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNER_ADDRESS_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNER_ADDWO_CONT_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNER_ADD_WITHOUT_CONTACT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNER_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNMENT_STATUS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSIGNOR_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOLE_DELIVERY_MODE_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOLE_MARKS_N_NUMBERS_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOLE_PAYMENT_TERMS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOLE_PAYMENT_TERMS_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOLE_VOLUME_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOLE_VOLUME_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOLE_WEIGHT_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOLE_WEIGHT_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOLIDATED_VOLUME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOLIDATED_WEIGHT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_DESTINATION_AIRPORT_CODE_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_FIRST_FLIGHT_AND_DAY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_ORIGIN_AIRPORT_CODE_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONSOL_SECOND_FLIGHT_AND_DAY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTACT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTACT_PERSON_ALIAS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTACT_PHONE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTAINER_COUNT_WITH_ETC_COUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTAINER_NUMBER2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTAINER_NUMBER_WITH_ETC_COUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTAINER_SUMMARY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CONTAINER_TYPE_CODE2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COPY_AS_AGREED;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COPY_CHARGES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.COUNTRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CREDITOR_AGENT_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CTO_FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CURRENCY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CUSTOMS_REFERENCE_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CUSTOMS_REFERENCE_NUMBER_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.CY_NAME_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.C_DESTINATION_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.C_ORIGIN_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DANGEROUS_GOODS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DELIVERY_INSTRUCTIONS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DELIVERY_TO_ADDRESS_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DELIVERY_TO_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DELIVERY_TRANSPORT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_AGENT_RN_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_CODE_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_COUNTRY_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DESTINATION_PORT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DG_CLASS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DG_EMERGENCY_CONTACT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DG_SUBSTANCE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DLV;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.DO_MESSAGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EMAIL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ERN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EXEMPTION_CARGO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EXP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EXPORTER_REFERENCE_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.EXPORTER_REFERENCE_NUMBER_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FLASH_POINT_AND_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FORWARDER_REFERENCE_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FORWARDER_REFERENCE_NUMBER_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FREIGHT_DOCUMENT_PERMISSION_EXCEPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FRN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.FULL_NAME1;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GOODS_DESCRIPTION2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GOODS_VALUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GROSS_VOLUME2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GROSS_VOLUME_UNIT2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GROSS_WEIGHT2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.GROSS_WEIGHT_UNIT2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.HAS_CHARGES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.HAS_CONTAINERS2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.HAS_PACKS2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.HAS_PACK_DETAILS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.HS_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.IMP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.INSURANCE_VALUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.IS_DG;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.IS_SECURITY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KC;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KCRA_EXPIRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KCRA_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KC_EXPIRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KC_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.KNOWN_CONSIGNOR;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.LOADED_DATE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.LOAD_DESCRIPTION_REMARKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MARINE_POLLUTANT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MARKS_NUMS2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MAWB_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MAX_TEMP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MAX_TEMP_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MIN_TEMP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MIN_TEMP_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.MOBILE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.NET_WEIGHT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.NET_WEIGHT_UNIT_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.NOTIFY_PARTY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.NOTIFY_PARTY_ADDRESS_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.NOTIFY_PARTY_ADD_WITHOUT_CONTACT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.NOTIFY_PARTY_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.NOTIFY_PART_ADDWO_CONT_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.NO_OF_PACKAGES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.OCEAN_DG_CLASS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.OCEAN_DG_PSN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.OCEAN_UN_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ONE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORDER_MANAGEMENT_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORG_DATA_NOT_AVAILABLE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORG_FULL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGINAL_PRINT_DATE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGIN_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGIN_AGENT_RN_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGIN_CODE_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ORIGIN_PORT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.OVERSEAS_SELL_AMOUNT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKAGES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKING_GROUP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS_COMMODITY_GROUP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS_CONTAINER_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS_DETAILS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS_GOODS_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS_GOODS_DESCRIPTION2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS_MARKS_NUMBER2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS_MARKS_NUMBERS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACKS_UNIT2;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PACK_SUMMARY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PAYMENT_TERMS_DESCRITION_WITH_CAMELCASE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PICKUP_FROM_ADDRESS_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PICKUP_FROM_LL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PICKUP_INSTRUCTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PICKUP_SHIPPERS_REF;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.POD_CODE_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.POD_COUNTRY_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.POD_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.POL_CODE_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.POL_COUNTRY_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.POL_COUNTRY_NAME_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.POL_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PRE_CARRIAGE_PARTY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.PRINT_USER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.RA;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.RAILAGE_PLAYER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.RA_CSD;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.RA_EXPIRY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.RA_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.REFERENCE_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.REGULATED_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SCI;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SCREENING_CODES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SEA;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SECURITY_STATUS_RECEIVED_FROM;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_CARGO_TYPE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_CONTAINERS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_DESTINATION_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_DETAIL_DATE_OF_ISSUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_DETAIL_DATE_OF_ISSUE_IN_CAPS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_FIRST_FLIGHT_AND_DAY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_ID;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_NOTIFY_PARTY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_ORIGIN_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_PACKING_HEIGHT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_PACKING_HEIGHT_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_PACKING_LENGTH;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_PACKING_LENGTH_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_PACKING_PACKS_TYPE_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_PACKING_WIDTH;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_PACKING_WIDTH_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_PACKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_REEFER_FLAG;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SHIPMENT_SECOND_FLIGHT_AND_DAY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SI_CUT_OFF_TIME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SPH;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.SRN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.STATE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_BOE_DATE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_BOE_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_BOOKING_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_BOOKING_AGENT_AWB_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_BOOKING_AGENT_BKG_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_BOOKING_AGENT_BL_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_BORROWED_FROM;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_CONTROLLED;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_CONTROLLED_REF_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_CO_LOADER_AWB_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_CO_LOADER_BKG_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_CO_LOADER_BL_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_CO_LOADER_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_CUSTOM_BROKERAGE_AT_DESTINATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_CUSTOM_BROKERAGE_AT_ORIGIN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_DELIVERY_AT_DESTINATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_DG;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_DG_FLAG;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_DRIVER_ID;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_DRIVER_MOBILE_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_DRIVER_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_EAR_DROP_OFF;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_EAR_EPY_EQ_PICK;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_FIRST_CARRIER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_FIRST_FLIGHT_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_FIRST_VESSEL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_FIRST_VOYAGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_INCOTERM_LOCATION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_LAST_CARRIER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_LAST_FLIGHT_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_LAST_VESSEL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_LAST_VOYAGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_LAT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_LAT_FULL_EQ_DELI;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_OWNERSHIP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_OWNERSHIP_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_PARTNER_DROP_DOWN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_PASSED_BY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_PASSED_BY_PERSON;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_PICKUP_AT_ORIGIN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_REEFER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_REEFER_FLAG;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_R_P_FIRST_VESSEL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_R_P_FIRST_VOYAGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_R_P_LAST_VESSEL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_R_P_LAST_VOYAGE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_SI;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_TERMINAL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_TRAILER_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_TRANSPORTER_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_TRANSPORT_BY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_TRUCK_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_TRUCK_TRAILER_TYPE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.S_VGM;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TERMINAL;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_ACTUALDELIVERY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_ACTUALPICKUP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_DELIVERTO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_DELIVERTOADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_DELIVERTOCONTACT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_DELIVERY_AGENT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_DELIVERY_AGENT_ADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_DELIVERY_AGENT_CONTACT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_DELIVERY_GATEIN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_DELIVERY_GATEOUT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_DROPMODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_ESTIMATEDDELIVERY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_ESTIMATEDPICKUP;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_FLIGHT_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_INSTRUCTIONTYPE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_ISAIR;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_ISSEA;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_PICKUPFROM;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_PICKUPFROMADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_PICKUPFROMCONTACT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_PICKUP_GATEIN;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_PICKUP_GATEOUT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_PORTTRANSPORTADVISED;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_REMARKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_REQUIREDBY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_TRANSPORTCOMPANY;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_TRANSPORTCOMPANYADDRESS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TI_TRANSPORTCOMPANYCONTACT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TWO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.TYPE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.UNID_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.US;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.USER_DISPLAY_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.USER_INITIALS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.USER_PHONE_NUMBER;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VALUE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VESSELS_NAME_FLIGHT_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VESSEL_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VOLUME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VOLUME_AND_UNIT_PACKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VOLUME_UNIT_DESCRIPTION;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VOLUME_WEIGHT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VOLUME_WEIGHT_UNIT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.VOYAGE_OR_FLIGHT_NO;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.V_WEIGHT_AND_UNIT_PACKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.WAREHOUSE_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.WAREHOUSE_NAME;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.WEIGHT;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.WEIGHT_AND_UNIT_PACKS;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.WITH_CONSIGNOR;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.YES;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants.ZIP_POST_CODE;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.combineStringsWithComma;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getAddressList;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getCityCountry;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getFormattedAddress;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.getOrgAddress;
import static com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper.numberToWords;
import static com.dpw.runner.shipment.services.commons.constants.Constants.DIRECTION_EXP;

import com.dpw.runner.shipment.services.ReportingService.CommonUtils.AmountNumberFormatter;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportConstants;
import com.dpw.runner.shipment.services.ReportingService.CommonUtils.ReportHelper;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ContainerCountByCode;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentAndContainerResponse;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentContainers;
import com.dpw.runner.shipment.services.ReportingService.Models.Commons.ShipmentResponse;
import com.dpw.runner.shipment.services.ReportingService.Models.IDocumentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.AdditionalDetailModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ArrivalDepartureDetailsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ConsolidationModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ContainerModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PackingModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PartiesModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.PickupDeliveryDetailsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ReferenceNumbersModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.RoutingsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.ShipmentOrderModel;
import com.dpw.runner.shipment.services.ReportingService.Models.ShipmentModel.TruckDriverDetailsModel;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IBillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.INPMServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.AwbConstants;
import com.dpw.runner.shipment.services.commons.constants.CacheConstants;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.EntityTransferConstants;
import com.dpw.runner.shipment.services.commons.constants.MasterDataConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.config.CustomKeyGenerator;
import com.dpw.runner.shipment.services.config.LocalTimeZoneHelper;
import com.dpw.runner.shipment.services.dao.interfaces.IAwbDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsoleShipmentMappingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IConsolidationDetailsDao;
import com.dpw.runner.shipment.services.dao.interfaces.IContainerDao;
import com.dpw.runner.shipment.services.dao.interfaces.IHblDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentSettingsDao;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ContainerSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.PackSummaryResponse;
import com.dpw.runner.shipment.services.dto.CalculationAPIsDto.ShipmentMeasurementDetailsDto;
import com.dpw.runner.shipment.services.dto.GeneralAPIRequests.CarrierListObject;
import com.dpw.runner.shipment.services.dto.request.HblPartyDto;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.awb.AwbGoodsDescriptionInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbNotifyPartyInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbShipmentInfo;
import com.dpw.runner.shipment.services.dto.request.awb.AwbSpecialHandlingCodesMappingInfo;
import com.dpw.runner.shipment.services.dto.request.billing.BillChargesFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ChargeTypeFilterRequest;
import com.dpw.runner.shipment.services.dto.request.hbl.HblContainerDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblDataDto;
import com.dpw.runner.shipment.services.dto.request.hbl.HblFreightsAndCharges;
import com.dpw.runner.shipment.services.dto.request.npm.NPMFetchMultiLangChargeCodeRequest;
import com.dpw.runner.shipment.services.dto.response.billing.BillBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse.BillChargeCostDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse.BillChargeRevenueDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse.CurrencyExchangeRateDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse.ExchangeRateType;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse.TaxDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse.TaxType;
import com.dpw.runner.shipment.services.dto.response.billing.ChargeTypeBaseResponse;
import com.dpw.runner.shipment.services.dto.response.npm.NPMFetchLangChargeCodeResponse;
import com.dpw.runner.shipment.services.dto.v1.request.AddressTranslationRequest;
import com.dpw.runner.shipment.services.dto.v1.response.AddressDataV1;
import com.dpw.runner.shipment.services.dto.v1.response.AddressTranslationListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.OrgAddressResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.WareHouseResponse;
import com.dpw.runner.shipment.services.entity.AchievedQuantities;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.Allocations;
import com.dpw.runner.shipment.services.entity.Awb;
import com.dpw.runner.shipment.services.entity.ConsoleShipmentMapping;
import com.dpw.runner.shipment.services.entity.ConsolidationDetails;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.Hbl;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.entity.TriangulationPartner;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.DigitGrouping;
import com.dpw.runner.shipment.services.entity.enums.GroupingNumber;
import com.dpw.runner.shipment.services.entity.enums.OceanDGStatus;
import com.dpw.runner.shipment.services.entity.enums.Ownership;
import com.dpw.runner.shipment.services.entity.enums.RoutingCarriage;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCommodityType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferMasterLists;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferUnLocations;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.TranslationException;
import com.dpw.runner.shipment.services.exception.exceptions.ValidationException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.dto.CarrierMasterData;
import com.dpw.runner.shipment.services.masterdata.dto.MasterData;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequest;
import com.dpw.runner.shipment.services.masterdata.dto.request.MasterListRequestV2;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.masterdata.request.ShipmentGuidRequest;
import com.dpw.runner.shipment.services.masterdata.response.ArObjectResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillChargesResponse;
import com.dpw.runner.shipment.services.masterdata.response.BillingResponse;
import com.dpw.runner.shipment.services.masterdata.response.ChargeTypesResponse;
import com.dpw.runner.shipment.services.masterdata.response.CommodityResponse;
import com.dpw.runner.shipment.services.masterdata.response.UnlocationsResponse;
import com.dpw.runner.shipment.services.masterdata.response.VesselsResponse;
import com.dpw.runner.shipment.services.repository.interfaces.IAwbRepository;
import com.dpw.runner.shipment.services.service.impl.ShipmentServiceImplV3;
import com.dpw.runner.shipment.services.service.interfaces.IAwbService;
import com.dpw.runner.shipment.services.service.interfaces.IContainerService;
import com.dpw.runner.shipment.services.service.interfaces.IPackingService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.AwbUtility;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.CountryListHelper.ISO3166;
import com.dpw.runner.shipment.services.utils.MasterDataUtils;
import com.dpw.runner.shipment.services.utils.StringUtility;
import com.fasterxml.jackson.core.type.TypeReference;
import com.google.common.base.Strings;
import com.nimbusds.jose.util.Pair;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;

@Slf4j
@SuppressWarnings({"unchecked", "java:S2259"})
public abstract class IReport {


    public static final String LOCAL_NAME = "LocalName";
    public static final String REGEX_S_S = "%s %s";
    @Autowired
    private IShipmentDao shipmentDao;

    @Autowired
    private IContainerDao containerDao;

    @Autowired
    private IConsolidationDetailsDao consolidationDetailsDao;

    @Autowired
    private BillingServiceUrlConfig billingServiceUrlConfig;

    @Autowired
    private IHblDao hblDao;

    @Autowired
    private JsonHelper jsonHelper;

    @Autowired
    private IAwbRepository awbRepository;

    @Autowired
    private ModelMapper modelMapper;

    @Autowired
    private IConsoleShipmentMappingDao consoleShipmentMappingDao;

    @Autowired
    private IShipmentSettingsDao shipmentSettingsDao;

    @Autowired
    private IAwbDao awbDao;
    @Autowired
    private INPMServiceAdapter npmServiceAdapter;

    @Autowired
    MasterDataFactory masterDataFactory;
    @Autowired
    private IBillingServiceAdapter billingServiceAdapter;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    CacheManager cacheManager;
    @Autowired
    public MasterDataUtils masterDataUtils;
    @Autowired
    CustomKeyGenerator keyGenerator;

    @Autowired
    private IContainerService containerService;
    @Autowired
    private IAwbService awbService;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private IPackingService packingService;

    @Autowired
    private ShipmentServiceImplV3 shipmentServiceImplV3;
    @Autowired
    private CommonUtils commonUtils;

    public static final String TEN_LAKH = "1000000";

    public abstract Map<String, Object> getData(Long id) throws RunnerException;
    abstract IDocumentModel getDocumentModel(Long id) throws RunnerException;
    abstract Map<String, Object> populateDictionary(IDocumentModel documentModel) throws RunnerException;

    public ShipmentContainers getShipmentContainer(ContainerModel row)
    {
        ShipmentContainers ship = new ShipmentContainers();
        ship.ContainerNumber = row.getContainerNumber();
        ship.SealNumber = StringUtility.isEmpty(row.getCarrierSealNumber()) ? row.getShipperSealNumber() : row.getCarrierSealNumber();
        ship.NoofPackages = isStringNullOrEmpty(row.getPacks()) ? null : Long.valueOf(row.getPacks());
        if(row.getPacks() != null && !row.getPacks().isEmpty())
            ship.ShipmentPacks = Long.valueOf(row.getPacks());
        ship.ShipmentPacksUnit = row.getPacksType();
        ship.GrossWeight = row.getGrossWeight();
        ship.GrossWeightUnit = row.getGrossWeightUnit();
        ship.TareWeight =  row.getTareWeight();
        ship.TareWeightUnit = row.getTareWeightUnit();
        ship.Measurement = getRoundedBigDecimal(row.getMeasurement(),2, RoundingMode.HALF_UP);
        ship.MeasurementUnit = row.getMeasurementUnit();
        ship.GrossVolume = row.getGrossVolume();
        ship.GrossVolumeUnit = row.getGrossVolumeUnit();
        ship.ContainerTypeCode = row.getContainerCode();
        ship.ContainerCount = row.getContainerCount();
        ship.ShipmentMarksnNums = row.getMarksNums();
        ship.NetWeight = row.getNetWeight();
        ship.NetWeightUnit = row.getNetWeightUnit();
        ship.MinTemp = getRoundedBigDecimal(row.getMinTemp(),2, RoundingMode.HALF_UP);
        ship.MinTempUnit = row.getMinTempUnit();
        ship.ShipmentHblDeliveryMode = row.getHblDeliveryMode();
        ship.DescriptionOfGoods = row.getDescriptionOfGoods();
        ship.CarrierSealNumber = row.getCarrierSealNumber();
        ship.CustomsSealNumber = row.getCustomsSealNumber();
        ship.ShipperSealNumber = row.getShipperSealNumber();
        ship.HazardousUn = row.getHazardousUn();
        ship.CargoGrossWeightUnit = String.format(REGEX_S_S, convertToWeightNumberFormat(row.getGrossWeight(), getCurrentTenantSettings()), row.getGrossWeightUnit());
        ship.OceanUNNumber = row.getUnNumber();
        ship.OceanDGPSN = row.getProperShippingName();
        if(!Objects.isNull(row.getMinimumFlashPoint()))
        {
            ship.FlashPointAndUnit = String.valueOf(row.getMinimumFlashPoint());
            if(!StringUtility.isEmpty(row.getMinimumFlashPointUnit()))
                ship.FlashPointAndUnit = ship.FlashPointAndUnit + " " + row.getMinimumFlashPointUnit();
        }
        ship.MarinePollutant = Boolean.TRUE.equals(row.getMarinePollutant()) ? "Marine Pollutant" : null;
        try {
            processMasterListRequests(row, ship);
            ship.VolumeUnitDescription = getMasterListItemDesc(ship.GrossVolumeUnit, MasterDataType.VOLUME_UNIT.name(), false);
            ship.WeightUnitDescription = getMasterListItemDesc(ship.GrossWeightUnit, MasterDataType.WEIGHT_UNIT.name(), false);
            ship.PacksUnitDescription = getMasterListItemDesc(ship.ShipmentPacksUnit, MasterDataType.PACKS_UNIT.name(), false);
            ship.PacksUnitDesc = ship.PacksUnitDescription;
            ship.PackingGroup = getMasterListItemDesc(row.getPackingGroup(), MasterDataType.PACKING_GROUP.name(), true);
            ship.OceanDGClass = getMasterListItemDesc(row.getDgClass(), MasterDataType.DG_CLASS.name(), true);
            ship.DgClassDescription = ship.OceanDGClass;
            if (row.getGrossWeight() != null && row.getTareWeight() != null)
                ship.VGMWeight = row.getGrossWeight().add(row.getTareWeight());
        } catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
        CommodityResponse commodityResponse = getCommodity(row.getCommodityCode());
        if (commodityResponse != null) {
            ship.CommodityDescription = commodityResponse.getDescription();
            ship.CommodityDescriptionWithHSCode = commodityResponse.getCommodityDescriptionWithHSCode();
        }
        if(row.getCommodityGroup() != null) {
            MasterData commodity = getMasterListData(MasterDataType.COMMODITY_GROUP, row.getCommodityGroup());
            if (commodity != null)
                ship.CommodityGroup = commodity.getItemDescription();
        }
        return ship;
    }

    private void processMasterListRequests(ContainerModel row, ShipmentContainers ship) {
        Set<MasterListRequest> requests = new HashSet<>();
        Cache cache = cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA);
        Cache.ValueWrapper value1 = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, ship.GrossVolumeUnit));
        if(Objects.isNull(value1))
            requests.add(MasterListRequest.builder().ItemType(MasterDataType.VOLUME_UNIT.getDescription()).ItemValue(ship.GrossVolumeUnit).Cascade(null).build());

        Cache.ValueWrapper value2 = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, ship.GrossWeightUnit));
        if(Objects.isNull(value2))
            requests.add(MasterListRequest.builder().ItemType(MasterDataType.WEIGHT_UNIT.getDescription()).ItemValue(ship.GrossWeightUnit).Cascade(null).build());

        Cache.ValueWrapper value3 = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, ship.ShipmentPacksUnit));
        if(Objects.isNull(value3))
            requests.add(MasterListRequest.builder().ItemType(MasterDataType.PACKS_UNIT.getDescription()).ItemValue(ship.ShipmentPacksUnit).Cascade(null).build());

        Cache.ValueWrapper value4 = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, row.getDgClass()));
        if(Objects.isNull(value4))
            requests.add(MasterListRequest.builder().ItemType(MasterDataType.DG_CLASS.getDescription()).ItemValue(row.getDgClass()).Cascade(null).build());

        Cache.ValueWrapper value5 = cache.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, row.getPackingGroup()));
        if(Objects.isNull(value5))
            requests.add(MasterListRequest.builder().ItemType(MasterDataType.PACKING_GROUP.getDescription()).ItemValue(row.getPackingGroup()).Cascade(null).build());

        if(!requests.isEmpty()) {
            MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
            masterListRequestV2.setMasterListRequests(requests.stream().toList());
            masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", MasterDataConstants.ITEM_VALUE, "ItemDescription", "ValuenDesc", "Cascade"));
            Set<String> keys = new HashSet<>();
            Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
            commonUtils.createMasterDataKeysList(requests, keys);
            masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), null);
        }
    }

    public String getMasterListItemDesc(String value, String type, boolean isValueNDesc) {
        if(StringUtility.isEmpty(value))
            return value;
        String key = value + "#" + type;
        Optional<Cache> cacheOptional = Optional.ofNullable(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA));
        var valueMapper = cacheOptional.map(c -> c.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, key))).orElse(null);
        if(!Objects.isNull(valueMapper)) {
            EntityTransferMasterLists object = (EntityTransferMasterLists) valueMapper.get();
            if(isValueNDesc)
            {
                if(!Objects.isNull(object) && !StringUtility.isEmpty(object.getValuenDesc()))
                    return object.getValuenDesc();
                return value;
            }
            String val = value;
            if(!Objects.isNull(object) && object.getItemDescription() != null)
                val = object.getItemDescription();
            return val.toUpperCase();
        }
        return value;
    }

    public String getTotalPackUnitCode(List<ShipmentModel> shipmentModelList) {
        String unitCode = "";
        for (var c : shipmentModelList) {
            if (c.getPacksUnit() != null && !c.getPacksUnit().isEmpty()) {
                if (!unitCode.equals(c.getPacksUnit()))
                    unitCode = "MPK";
            } else {
                if (unitCode.equals("MPK"))
                    unitCode = c.getPacksUnit();
            }
        }
        return unitCode;
    }

    public void populateBLContainer(ShipmentContainers shipmentContainer, HblContainerDto blObjectContainer) {
        ShipmentSettingsDetails shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        V1TenantSettingsResponse tenantSettings = getCurrentTenantSettings();
        Integer decimalPlaces = shipmentSettingsDetails.getDecimalPlaces();
        if(decimalPlaces == null)
            decimalPlaces = 2;
        shipmentContainer.BL_ContainerType = blObjectContainer.getContainerType();
        shipmentContainer.BL_SealNumber = blObjectContainer.getSealNumber();
        if (blObjectContainer.getContainerGrossWeight() != null)
            shipmentContainer.BL_GrossWeight = getDPWWeightVolumeFormat(blObjectContainer.getContainerGrossWeight(), decimalPlaces, tenantSettings);
        else
            shipmentContainer.BL_GrossWeight = Constants.EMPTY_STRING;
        shipmentContainer.BL_GrossWeightUnit = blObjectContainer.getContainerGrossWeightUnit();
        if (blObjectContainer.getContainerGrossVolume() != null)
            shipmentContainer.BL_GrossVolume = getDPWWeightVolumeFormat(blObjectContainer.getContainerGrossVolume(), decimalPlaces, tenantSettings);
        else
            shipmentContainer.BL_GrossVolume = Constants.EMPTY_STRING;
        shipmentContainer.BL_GrossVolumeUnit = blObjectContainer.getContainerGrossVolumeUnit();
        shipmentContainer.BL_NoofPackages = blObjectContainer.getNoOfPackages();
        shipmentContainer.BL_CarrierSealNumber = blObjectContainer.getCarrierSealNumber();
        shipmentContainer.BL_ContainerNumber = blObjectContainer.getContainerNumber();
        shipmentContainer.BL_ContainerDescription = blObjectContainer.getContainerDesc();
        shipmentContainer.BL_PackageUnit = blObjectContainer.getPackageUnit();
    }

    public void populateShipmentFields(ShipmentModel shipment, Map<String, Object> dictionary)
    {
        if (shipment == null) {
            return;
        }
        var shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        PickupDeliveryDetailsModel pickup = shipment.getPickupDetails();
        PickupDeliveryDetailsModel delivery = shipment.getDeliveryDetails();

        if(shipment.getTransportMode() != null) {
            dictionary.put(TI_ISSEA, shipment.getTransportMode().equals(SEA));
            dictionary.put(TI_ISAIR, shipment.getTransportMode().equals(AIR));
        }

        if(shipment.getCarrierDetails() != null)
            dictionary.put(TI_FLIGHT_NUMBER, shipment.getCarrierDetails().getFlightNumber());

        if(shipment.getOrderManagementNumber()!=null)
            dictionary.put(ORDER_MANAGEMENT_NUMBER, shipment.getOrderManagementNumber());

        populateShipmentOrders(shipment, dictionary);

        if(shipment.getTransportInstructionId() != null)
            addTransportInstructionTags(dictionary , shipment);
        PartiesModel shipmentClient = shipment.getClient();
        PartiesModel shipmentConsignee = shipment.getConsignee();
        PartiesModel shipmentConsigner = shipment.getConsigner();
        AdditionalDetailModel additionalDetails = new AdditionalDetailModel();
        if(shipment.getAdditionalDetails() != null) {
            additionalDetails = shipment.getAdditionalDetails();
        }
        // UnLocations Master-data
        Map<String, UnlocationsResponse> unlocationsMap = getUnlocationsResponseMap(shipment);
        // Master lists Master-data
        List<MasterListRequest> masterListRequest = createMasterListsRequestFromShipment(shipment);
        masterListRequest.addAll(createMasterListsRequestFromUnLocoMap(unlocationsMap));
        Map<Integer, Map<String, MasterData>> masterListsMap = getMasterListsMap(masterListRequest);
        PartiesModel shipmentNotify = additionalDetails.getNotifyParty();
        if (shipment.getReferenceNumbersList() != null) {
            dictionary.put(AMS_NUMBER, shipment.getReferenceNumbersList().stream()
                .filter(i -> i.getType().equalsIgnoreCase(AMS))
                .findFirst()
                .map(ReferenceNumbersModel::getReferenceNumber)
                .orElse(null));

            dictionary.put(SRN, shipment.getReferenceNumbersList().stream()
                .filter(i -> i.getType().equalsIgnoreCase(SRN))
                .findFirst()
                .map(ReferenceNumbersModel::getReferenceNumber)
                .orElse(null));
        }

        if (shipment.getShipmentAddresses() != null) {
            dictionary.put(CARGO_LOCATION, shipment.getShipmentAddresses().stream()
                .filter(i -> i.getType().equalsIgnoreCase(CAL))
                .findFirst()
                .map(ReportHelper::getOrgAddressDetails)
                .orElse(null));
        }
        UnlocationsResponse pol = unlocationsMap.get(shipment.getCarrierDetails().getOriginPort());
        UnlocationsResponse pod = unlocationsMap.get(shipment.getCarrierDetails().getDestinationPort());
        UnlocationsResponse origin = unlocationsMap.get(shipment.getCarrierDetails().getOrigin());
        UnlocationsResponse destination = unlocationsMap.get(shipment.getCarrierDetails().getDestination());
        UnlocationsResponse paidPlace = unlocationsMap.get(shipment.getAdditionalDetails().getPaidPlace());
        UnlocationsResponse placeOfIssue = unlocationsMap.get(shipment.getAdditionalDetails().getPlaceOfIssue());
        Map<String, WareHouseResponse> wareHouseResponseMap = masterDataUtils.fetchWareHouseData(
                Arrays.asList(shipment.getAdditionalDetails().getWarehouseId(), shipment.getAdditionalDetails().getBondedWarehouseId()));
        dictionary.put(REFERENCE_NO, shipment.getBookingReference());
        dictionary.put(ReportConstants.MASTER_BILL,shipment.getMasterBill());
        dictionary.put(ReportConstants.HOUSE_BILL,shipment.getHouseBill());
        dictionary.put(SHIPMENT_ID, shipment.getShipmentId());
        String carrierVesselId = shipment.getCarrierDetails().getVessel();
        Set<String> vesselIds = new HashSet<>();
        processVessels(shipment, dictionary, vesselIds, carrierVesselId);
        dictionary.put(ReportConstants.VOYAGE,shipment.getCarrierDetails().getVoyage());
        processPolPodTags(shipment, dictionary, pol, pod, additionalDetails, masterListsMap);
        dictionary.put(ReportConstants.PLACE_OF_DELIVERY, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.REFERENCE_NUMBER,shipment.getBookingReference());
        processOriginTags(dictionary, origin, masterListsMap);
        dictionary.put(ReportConstants.DESCRIPTION,shipment.getGoodsDescription());
        dictionary.put(ReportConstants.SHIPMENT_TYPE,shipment.getDirection());
        dictionary.put(ReportConstants.CUSTOM_SHIPMENT_TYPE, shipment.getDirection() != null ? Character.toUpperCase(shipment.getDirection().charAt(0)) : null);
        int containerCount = getContainerCount(shipment);
        dictionary.put(ReportConstants.CONTAINER_COUNT, numberToWords(containerCount).toUpperCase());
        dictionary.put(PICKUP_INSTRUCTION, shipment.getPickupDetails() != null ? shipment.getPickupDetails().getPickupDeliveryInstruction() : null);
        dictionary.put(DELIVERY_INSTRUCTIONS, shipment.getDeliveryDetails() != null ? shipment.getDeliveryDetails().getPickupDeliveryInstruction() : null);
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        processDateTimeTags(shipment, dictionary, tsDateTimeFormat, v1TenantSettingsResponse, additionalDetails, pickup);

        populateShippedOnboardFields(shipment, dictionary);
        populateReeferFields(shipment, dictionary);
        dictionary.put(SHIPMENT_CARGO_TYPE, shipment.getShipmentType());

        dictionary.put(ReportConstants.INCO_TERM, shipment.getIncoterms());
        dictionary.put(ReportConstants.INCOTERM, shipment.getIncoterms());
        dictionary.put(ReportConstants.CHARGEABLE, convertToWeightNumberFormat(shipment.getChargable(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CHARGEABLE_UNIT, shipment.getChargeableUnit());
        dictionary.put(ReportConstants.TRANSPORT_MODE, shipment.getTransportMode());
        dictionary.put(ReportConstants.CONTAINER_TYPE, shipment.getShipmentType());

        addOtherTagsAndProcessMasterData(shipment, dictionary, masterListsMap, v1TenantSettingsResponse, delivery, pickup, additionalDetails, placeOfIssue, paidPlace, destination, tsDateTimeFormat, pol, pod);
        processTransportModeTags(shipment, dictionary, shipmentConsigner, shipmentConsignee, shipmentNotify, shipmentClient, additionalDetails, tsDateTimeFormat, shipmentSettingsDetails);
        processReferenceNumbersList(shipment, dictionary);
        processShippingLineTags(shipment, dictionary);
        addPreCarriageTags(dictionary, pickup);
        addNoOfPacks(shipment, dictionary, v1TenantSettingsResponse);
        addWareHouseNameAndCodeTags(shipment, dictionary, wareHouseResponseMap);
        if(shipment.getPickupDetails() != null)
            dictionary.put(PICKUP_SHIPPERS_REF, shipment.getPickupDetails().getShipperRef());
        processDeliveryTransportTag(shipment, dictionary);
        setContainerCount(shipment, dictionary);
        populateUserFields(UserContext.getUser(), dictionary);
        populateHasContainerFields(shipment, dictionary, v1TenantSettingsResponse);
        addVoyageFlightNoTag(shipment, dictionary);
        addGoodAndInsuranceValueTag(dictionary, v1TenantSettingsResponse);
        addAliasTag(shipment, dictionary, v1TenantSettingsResponse);
        populateIGMInfo(shipment, dictionary);
        addDgTags(shipment, dictionary);
        dictionary.put(MAWB_CAPS, StringUtility.convertToString(shipment.getMasterBill()));
        populateV3TruckDriverDetailsTags(shipment, dictionary);
    }

    private void addNoOfPacks(ShipmentModel shipment, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(shipment.getNoOfPacks() != null) {
            dictionary.put(ReportConstants.NO_OF_PACKAGES, getDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getNoOfPacks()), 0, v1TenantSettingsResponse));
            dictionary.put(ReportConstants.NO_OF_PACKAGES_IN_CAPS, StringUtility.toUpperCase(getDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getNoOfPacks()), 0, v1TenantSettingsResponse)));
        }
    }

    private void addPreCarriageTags(Map<String, Object> dictionary, PickupDeliveryDetailsModel pickup) {
        if(!Objects.isNull(pickup) && !Objects.isNull(pickup.getTransporterDetail())){
            dictionary.put(PRE_CARRIAGE_PARTY, pickup.getTransporterDetail().getOrgData() != null &&
                    pickup.getTransporterDetail().getOrgData().containsKey(FULL_NAME1) ?
                    pickup.getTransporterDetail().getOrgData().get(FULL_NAME1) : "");
        }
    }

    public void populateShippedOnboardFields(ShipmentModel shipmentModel, Map<String, Object> dictionary) {

        if (Objects.isNull(shipmentModel) || Objects.isNull(shipmentModel.getAdditionalDetails())) return;

        LocalDateTime shippedOnboardDate = shipmentModel.getAdditionalDetails().getShippedOnboard();
        if (Objects.nonNull(shippedOnboardDate)) {
            dictionary.put(ReportConstants.SHIPPED_ONBOARD_TEXT, ReportConstants.SHIPPED_ONBOARD);
            dictionary.put(ReportConstants.SHIPPED_ONBOARD_DATE_DDMMMYYYY, convertToDPWDateFormat(
                    shippedOnboardDate, "ddMMMyyyy", false));
        }
    }

    private void addWareHouseNameAndCodeTags(ShipmentModel shipment, Map<String, Object> dictionary, Map<String, WareHouseResponse> wareHouseResponseMap) {
        if (!Objects.isNull(shipment.getAdditionalDetails().getWarehouseId()) &&
                wareHouseResponseMap.containsKey(StringUtility.convertToString(shipment.getAdditionalDetails().getWarehouseId()))) {
            dictionary.put(WAREHOUSE_NAME, wareHouseResponseMap.get(StringUtility.convertToString(shipment.getAdditionalDetails().getWarehouseId())).getWarehouseDepotName());
            dictionary.put(WAREHOUSE_CODE, wareHouseResponseMap.get(StringUtility.convertToString(shipment.getAdditionalDetails().getWarehouseId())).getWarehouseDepotCode());
        }
        if (!Objects.isNull(shipment.getAdditionalDetails().getBondedWarehouseId()) &&
                wareHouseResponseMap.containsKey(StringUtility.convertToString(shipment.getAdditionalDetails().getBondedWarehouseId()))) {
            dictionary.put(BOUNDED_WAREHOUSE_NAME, wareHouseResponseMap.get(StringUtility.convertToString(shipment.getAdditionalDetails().getBondedWarehouseId())).getWarehouseDepotName());
            dictionary.put(BOUNDED_WAREHOUSE_CODE, wareHouseResponseMap.get(StringUtility.convertToString(shipment.getAdditionalDetails().getBondedWarehouseId())).getWarehouseDepotCode());
        }
    }

    private void addGoodAndInsuranceValueTag(Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if (!Objects.isNull(dictionary.get(GOODS_VALUE)))
            dictionary.put(GOODS_VALUE, AmountNumberFormatter.format(new BigDecimal(StringUtility.convertToString(dictionary.get(GOODS_VALUE))), UserContext.getUser().getCompanyCurrency(), v1TenantSettingsResponse));
        if (!Objects.isNull(dictionary.get(INSURANCE_VALUE)))
            dictionary.put(INSURANCE_VALUE, AmountNumberFormatter.format(new BigDecimal(StringUtility.convertToString(dictionary.get(INSURANCE_VALUE))), UserContext.getUser().getCompanyCurrency(), v1TenantSettingsResponse));
    }

    private void addAliasTag(ShipmentModel shipment, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if (!Objects.isNull(shipment.getNoOfPacks()))
            dictionary.put(ReportConstants.NO_OF_PACKAGES_ALIAS, getDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getNoOfPacks()), 0, v1TenantSettingsResponse));
    }

    private void addDgTags(ShipmentModel shipment, Map<String, Object> dictionary) {
        dictionary.put(IS_DG, false);
        if(Boolean.TRUE.equals(shipment.getContainsHazardous())) {
            dictionary.put(IS_DG, true);
            dictionary.put(S_DG_FLAG, YES);
            dictionary.put(DG_EMERGENCY_CONTACT, getConcatenatedContact(shipment.getAdditionalDetails().getEmergencyContactNumberCode(), shipment.getAdditionalDetails().getEmergencyContactNumber()));
        }
    }

    public void populateReeferFields(ShipmentModel shipmentModel, Map<String, Object> dictionary) {

        if (Objects.isNull(shipmentModel)) return;
        dictionary.put(S_REEFER_FLAG, Boolean.FALSE);
        if (Boolean.TRUE.equals(shipmentModel.getIsReefer())) {
            dictionary.put(S_REEFER_FLAG, Boolean.TRUE);
            dictionary.put(SHIPMENT_REEFER_FLAG, YES);
        }
    }

    public void populateDGFields(ShipmentModel shipmentModel, Map<String, Object> dictionary) {

        if (Objects.isNull(shipmentModel)) return;
        dictionary.put(IS_DG, Boolean.FALSE);
        if (Boolean.TRUE.equals(shipmentModel.getContainsHazardous())) {
            dictionary.put(IS_DG, Boolean.TRUE);
            dictionary.put(S_DG_FLAG, YES);
        }
    }

    private void addVoyageFlightNoTag(ShipmentModel shipment, Map<String, Object> dictionary) {
        if(Objects.equals(shipment.getTransportMode(), AIR))
            dictionary.put(VOYAGE_OR_FLIGHT_NO, shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getFlightNumber() : null);
        else
            dictionary.put(VOYAGE_OR_FLIGHT_NO, shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getVoyage() : null);
    }

    private void processDateTimeTags(ShipmentModel shipment, Map<String, Object> dictionary, String tsDateTimeFormat, V1TenantSettingsResponse v1TenantSettingsResponse, AdditionalDetailModel additionalDetails, PickupDeliveryDetailsModel pickup) {
        dictionary.put(ReportConstants.ETA, convertToDPWDateFormat(shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getEta() : null, tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.ETD, convertToDPWDateFormat(shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getEtd() : null, tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.ATA, convertToDPWDateFormat(shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getAta() : null, tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.ATD, convertToDPWDateFormat(shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getAtd() : null, tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.DATE_OF_DEPARTURE, dictionary.get(ReportConstants.ATD) == null ? dictionary.get(ReportConstants.ETD) : dictionary.get(ReportConstants.ATD));
        dictionary.put(ReportConstants.SYSTEM_DATE, convertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.ONBOARD_DATE, convertToDPWDateFormat(additionalDetails.getOnBoardDate(), tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.ESTIMATED_READY_FOR_PICKUP, pickup != null ? pickup.getEstimatedPickupOrDelivery() : null);
        String formatPattern = "dd/MMM/y";
        if(!CommonUtils.isStringNullOrEmpty(v1TenantSettingsResponse.getDPWDateFormat()))
            formatPattern = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(ReportConstants.SHIPMENT_CREATION_DATE, convertToDPWDateFormat(shipment.getShipmentCreatedOn(), tsDateTimeFormat, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.DATE_OF_ISSUE, convertToDPWDateFormat(additionalDetails.getDateOfIssue(), formatPattern, true));
        dictionary.put(SHIPMENT_DETAIL_DATE_OF_ISSUE, convertToDPWDateFormat(additionalDetails.getDateOfIssue(), formatPattern, true));
        dictionary.put(SHIPMENT_DETAIL_DATE_OF_ISSUE_IN_CAPS, StringUtility.toUpperCase(convertToDPWDateFormat(additionalDetails.getDateOfIssue(), formatPattern, true)));
        dictionary.put(ReportConstants.DATE_OF_RECEIPT, additionalDetails.getDateOfReceipt());

        LocalDateTime ataOrEta = shipment.getCarrierDetails().getAta() != null ? shipment.getCarrierDetails().getAta() : shipment.getCarrierDetails().getEta();
        dictionary.put(ATA_OR_ETA, convertToDPWDateFormat(ataOrEta, tsDateTimeFormat, v1TenantSettingsResponse));
    }

    private void processShippingLineTags(ShipmentModel shipment, Map<String, Object> dictionary) {
        if(Strings.isNullOrEmpty(shipment.getCarrierDetails().getShippingLine())){
            return;
        }
        CarrierMasterData carrierData = getCarrier(shipment.getCarrierDetails().getShippingLine());
        if(!Objects.isNull(carrierData)) {
            dictionary.put(CARRIER_NAME, carrierData.getItemDescription());
            String iataCode = carrierData.getIataCode();
            dictionary.put(ReportConstants.FLIGHT_IATA_CODE, iataCode);
            dictionary.put(ReportConstants.IATA_CODE, StringUtility.isEmpty(iataCode) ? shipment.getCarrierDetails().getFlightNumber() : iataCode + getShipmentFlightNumber(shipment));
            dictionary.put(ReportConstants.SHIPMENT_FLIGHT_NUMBER_WITH_IATACODE, StringUtility.isEmpty(iataCode) ? shipment.getCarrierDetails().getFlightNumber() : iataCode + getShipmentFlightNumber(shipment));
        }
    }

    private String getShipmentFlightNumber(ShipmentModel shipment) {
        return StringUtility.isEmpty(shipment.getCarrierDetails().getFlightNumber()) ? "" : (" " + shipment.getCarrierDetails().getFlightNumber());
    }

    private void processOriginTags(Map<String, Object> dictionary, UnlocationsResponse origin, Map<Integer, Map<String, MasterData>> masterListsMap) {
        dictionary.put(ReportConstants.ORIGIN_NAME, origin != null ? origin.getName() : null);
        dictionary.put(ReportConstants.ORIGIN, origin != null ? origin.getName() : null);
        dictionary.put(ReportConstants.ORIGIN_COUNTRY, origin != null ? origin.getCountry() : null);
        if(origin != null) {
            dictionary.put(ReportConstants.ORIGIN_NAME_IN_CAPS, origin.getName().toUpperCase());
            String originCountry = masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(origin.getCountry()) ? masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(origin.getCountry()).getItemDescription() : "";
            dictionary.put(ReportConstants.ORIGIN_COUNTRY_NAME_IN_CAPS, originCountry.toUpperCase());
            dictionary.put(ReportConstants.POR_IN_CAPS, origin.getName().toUpperCase());
            dictionary.put(ReportConstants.POR_COUNTRY_NAME_IN_CAPS, originCountry.toUpperCase());
            dictionary.put(ORIGIN_CODE_IN_CAPS, StringUtility.toUpperCase(origin.getLocCode()));
        }
    }

    private void processDeliveryTransportTag(ShipmentModel shipment, Map<String, Object> dictionary) {
        PartiesModel deliveryTransport = null;
        if(shipment.getDeliveryDetails() != null)
            deliveryTransport = shipment.getDeliveryDetails().getTransporterDetail();
        if (deliveryTransport != null && deliveryTransport.getAddressData() != null)
        {
            Map<String, Object> addressMap = deliveryTransport.getAddressData();
            populateAddress(addressMap, dictionary, DELIVERY_TRANSPORT);
            var address = getOrgAddress(getValueFromMap(addressMap, ORG_FULL_NAME), getValueFromMap(addressMap, ADDRESS1), getValueFromMap(addressMap, ADDRESS2),
                    getCityCountry(getValueFromMap(addressMap, CITY), getValueFromMap(addressMap, COUNTRY)),
                    getValueFromMap(addressMap, EMAIL), getValueFromMap(addressMap, CONTACT_PHONE));
            dictionary.put(ReportConstants.DELIVERY_TRANSPORT, address);
        }
    }

    private void processReferenceNumbersList(ShipmentModel shipment, Map<String, Object> dictionary) {
        if(shipment.getReferenceNumbersList() != null) {
            List<String> referenceNumberList = shipment.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equals(ERN)).map(ReferenceNumbersModel::getReferenceNumber).toList();
            List<String> referenceNumberListInCaps = referenceNumberList.stream().map(StringUtility::toUpperCase).toList();
            if(!referenceNumberList.isEmpty()){
                dictionary.put(EXPORTER_REFERENCE_NUMBER, String.join(",", referenceNumberList));
            }
            if(!referenceNumberListInCaps.isEmpty()) {
                dictionary.put(EXPORTER_REFERENCE_NUMBER_IN_CAPS, String.join(",", referenceNumberListInCaps));
            }
        }
        if(shipment.getReferenceNumbersList() != null) {
            List<String> referenceNumberList;
            referenceNumberList = shipment.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equals(CEN)).map(ReferenceNumbersModel::getReferenceNumber).toList();
            List<String> referenceNumberListInCaps = referenceNumberList.stream().map(StringUtility::toUpperCase).toList();
            if(!referenceNumberList.isEmpty()){
                dictionary.put(CUSTOMS_REFERENCE_NUMBER, String.join(",", referenceNumberList));
            }
            if(!referenceNumberListInCaps.isEmpty()) {
                dictionary.put(CUSTOMS_REFERENCE_NUMBER_IN_CAPS, String.join(",", referenceNumberListInCaps));
            }
        }
        if(shipment.getReferenceNumbersList() != null) {
            List<String> referenceNumberList;
            referenceNumberList = shipment.getReferenceNumbersList().stream()
                    .filter(i -> i.getType().equals(FRN)).map(ReferenceNumbersModel::getReferenceNumber).toList();
            List<String> referenceNumberListInCaps = referenceNumberList.stream().map(StringUtility::toUpperCase).toList();
            if(!referenceNumberList.isEmpty()){
                dictionary.put(FORWARDER_REFERENCE_NUMBER, String.join(",", referenceNumberList));
            }
            if(!referenceNumberListInCaps.isEmpty()) {
                dictionary.put(FORWARDER_REFERENCE_NUMBER_IN_CAPS, String.join(",", referenceNumberListInCaps));
            }
        }
    }

    private void processTransportModeTags(ShipmentModel shipment, Map<String, Object> dictionary, PartiesModel shipmentConsigner, PartiesModel shipmentConsignee, PartiesModel shipmentNotify, PartiesModel shipmentClient, AdditionalDetailModel additionalDetails, String tsDateTimeFormat, ShipmentSettingsDetails shipmentSettingsDetails) {
        if ((Objects.equals(shipment.getTransportMode(), "SEA") || Objects.equals(shipment.getTransportMode(), "ROA") || Objects.equals(shipment.getTransportMode(), "RF") || Objects.equals(shipment.getTransportMode(), "AIR"))) {
            final String email = "Email";
            populateConsignerData(dictionary, shipmentConsigner, email);

            List<String> consignee = populateConsigneeData(dictionary, shipmentConsignee);
            dictionary.put(ReportConstants.CONSIGNEE,consignee);
            populateNotifyPartyData(dictionary, shipmentNotify, email);

            populateClientData(dictionary, shipmentClient, email);

            populateShipmentCargoManifestParty(shipment, dictionary);

            addNotifyAddressTag(shipment.getShipmentAddresses(), dictionary, SHIPMENT_NOTIFY_PARTY);

            processOriginAndDestinationAgentTags(dictionary, additionalDetails);

            processPickupDetailsTags(shipment, dictionary, tsDateTimeFormat, shipmentSettingsDetails, email);
            if(shipment.getReferenceNumbersList() != null)
            {
                List<String> referenceNumberList = shipment.getReferenceNumbersList().stream()
                        .filter(i -> i.getType().equals(DLV)).map(ReferenceNumbersModel::getReferenceNumber).toList();
                if(!referenceNumberList.isEmpty()){
                    dictionary.put(DO_MESSAGE, String.join(",", referenceNumberList));
                }
                referenceNumberList = shipment.getReferenceNumbersList().stream()
                        .filter(i -> i.getType().equals(RAILAGE_PLAYER)).map(ReferenceNumbersModel::getReferenceNumber).toList();
                if(!referenceNumberList.isEmpty()){
                    dictionary.put(RAILAGE_PLAYER, String.join(",", referenceNumberList));
                }
                referenceNumberList = shipment.getReferenceNumbersList().stream()
                        .filter(i -> i.getType().equals(ADDITIONAL_COST_AT)).map(ReferenceNumbersModel::getReferenceNumber).toList();
                if(!referenceNumberList.isEmpty()){
                    dictionary.put(ADDITIONAL_COST_AT, String.join(",", referenceNumberList));
                }
            }
        }
    }

    private void processOriginAndDestinationAgentTags(Map<String, Object> dictionary, AdditionalDetailModel additionalDetails) {
        PartiesModel originAgent = additionalDetails.getExportBroker();
        if(originAgent != null) {
            Map<String, Object> addressData = originAgent.getAddressData();
            if(addressData != null) {
                List<String> partyAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                        StringUtility.convertToString(addressData.get(ADDRESS2)),
                        ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                        null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                        StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
                );
                dictionary.put(SHIPMENT_ORIGIN_AGENT, partyAddress);
            }
        }

        PartiesModel destinationAgent = additionalDetails.getImportBroker();
        if(destinationAgent != null) {
            Map<String, Object> addressData = destinationAgent.getAddressData();
            if(addressData != null) {
                List<String> partyAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                        StringUtility.convertToString(addressData.get(ADDRESS2)),
                        ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                        null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                        StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
                );
                dictionary.put(SHIPMENT_DESTINATION_AGENT, partyAddress);
            }
        }
    }

    private void processPickupDetailsTags(ShipmentModel shipment, Map<String, Object> dictionary, String tsDateTimeFormat, ShipmentSettingsDetails shipmentSettingsDetails, String email) {
        if(shipment.getPickupDetails() != null && shipment.getPickupDetails().getSourceDetail() != null)
        {
            Map<String, Object> pickupAddress = shipment.getPickupDetails().getSourceDetail().getAddressData();
            List<String> pickupAddressList;
            if(pickupAddress != null)
            {
                pickupAddressList = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(pickupAddress, COMPANY_NAME), getValueFromMap(pickupAddress, ADDRESS1),
                        getValueFromMap(pickupAddress, ADDRESS2),
                        ReportHelper.getCityCountry(getValueFromMap(pickupAddress, CITY), getValueFromMap(pickupAddress, COUNTRY)),
                        getValueFromMap(pickupAddress, email), getValueFromMap(pickupAddress, CONTACT_PHONE),
                        getValueFromMap(pickupAddress,ZIP_POST_CODE)
                );
                String pickupName = getValueFromMap(pickupAddress, COMPANY_NAME);
                dictionary.put(ReportConstants.PICKUP_FROM,pickupName);
                dictionary.put(ReportConstants.PICKUP_FROM_IN_CAPS, pickupName != null ? pickupName.toUpperCase() : "");
                dictionary.put(ReportConstants.PICKUP_FROM_ADDRESS,pickupAddressList);
                dictionary.put(ReportConstants.PICKUP_FROM_ADDRESS_IN_CAPS, pickupAddressList.stream().map(String::toUpperCase).toList());

            }
        }
        if(shipment.getPickupDetails() != null) {
            processTagsWithActualEstimatedPickupOrDelivery(shipment, dictionary, tsDateTimeFormat);

            if(shipment.getPickupDetails().getDestinationDetail() != null) {
                List<String> cyNameAddress = new ArrayList<>();
                if(!Boolean.TRUE.equals(shipmentSettingsDetails.getDisableBlPartiesName()))
                    cyNameAddress.add(getValueFromMap(shipment.getPickupDetails().getDestinationDetail().getOrgData(), FULL_NAME));
                cyNameAddress.addAll(getOrgAddress(shipment.getPickupDetails().getDestinationDetail()));
                dictionary.put(CY_NAME_ADDRESS, String.join("\r\n", cyNameAddress));
            }

        }
    }

    private void processTagsWithActualEstimatedPickupOrDelivery(ShipmentModel shipment, Map<String, Object> dictionary, String tsDateTimeFormat) {
        if (shipment.getPickupDetails().getActualPickupOrDelivery() != null) {
            dictionary.put(ReportConstants.PICKUP_TIME, convertToDPWDateFormatWithTime(shipment.getPickupDetails().getActualPickupOrDelivery(), tsDateTimeFormat, true));
            dictionary.put(ReportConstants.PICKUPTIME_TYPE,  "Actual Pickup");
        } else {
            if (shipment.getPickupDetails().getEstimatedPickupOrDelivery() != null) {
                dictionary.put(ReportConstants.PICKUP_TIME, convertToDPWDateFormatWithTime(shipment.getPickupDetails().getEstimatedPickupOrDelivery(), tsDateTimeFormat, true));
            } else {
                dictionary.put(ReportConstants.PICKUP_TIME, "");
            }
            dictionary.put(ReportConstants.PICKUPTIME_TYPE, "Estimated Pickup");
        }
    }

    private void populateConsignerData(Map<String, Object> dictionary, PartiesModel shipmentConsigner, String email) {
        List<String> consigner = null;
        List<String> consignerWoCont = null;
        if(shipmentConsigner != null)
        {
            Map<String, Object> consignerAddress = shipmentConsigner.getAddressData();
            if(consignerAddress != null)
            {
                consigner = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consignerAddress, COMPANY_NAME), getValueFromMap(consignerAddress, ADDRESS1),
                        getValueFromMap(consignerAddress, ADDRESS2), ReportHelper.getCityCountry(getValueFromMap(consignerAddress, CITY), getValueFromMap(consignerAddress, COUNTRY)),
                        getValueFromMap(consignerAddress, email), getValueFromMap(consignerAddress, CONTACT_PHONE),
                        getValueFromMap(consignerAddress,ZIP_POST_CODE));

                consignerWoCont = ReportHelper.getOrgAddressWithoutPhoneEmail(
                        getValueFromMap(consignerAddress, COMPANY_NAME),
                        getValueFromMap(consignerAddress, ADDRESS1),
                        getValueFromMap(consignerAddress, ADDRESS2),
                        getValueFromMap(consignerAddress, CITY),
                        getValueFromMap(consignerAddress, STATE),
                        getValueFromMap(consignerAddress, ZIP_POST_CODE),
                        getValueFromMap(consignerAddress, COUNTRY)
                );

                dictionary.put(ReportConstants.CONSIGNER_NAME, consignerAddress.get(COMPANY_NAME));
                dictionary.put(ReportConstants.CONSIGNER_COMPANY_NAME, consignerAddress.get(COMPANY_NAME));
                dictionary.put(ReportConstants.CONSIGNER_CONTACT_PERSON, consignerAddress.get(CONTACT_PERSON_ALIAS));
                dictionary.put(ReportConstants.CONSIGNER_ADDRESS, ReportHelper.getOrgAddress(shipmentConsigner));

                try {
                    dictionary.put(ReportConstants.CONSIGNER_PHONE, consignerAddress.get("ContactPhone"));
                    dictionary.put(ReportConstants.CONSIGNER_FULL_NAME, shipmentConsigner.getOrgData().get(FULL_NAME1));
                } catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
            }
            if(shipmentConsigner.getOrgData() != null)
                dictionary.put(ReportConstants.CONSIGNER_LOCAL_NAME, shipmentConsigner.getOrgData().get(LOCAL_NAME));
            processConsignorAddressFreeText(dictionary, shipmentConsigner, consignerAddress, consigner);
        }
        dictionary.put(ReportConstants.CONSIGNER,consigner);
        dictionary.put(CONSIGNER_ADD_WITHOUT_CONTACT,consignerWoCont);
    }

    private void processConsignorAddressFreeText(Map<String, Object> dictionary, PartiesModel shipmentConsigner, Map<String, Object> consignerAddress, List<String> consigner) {
        List<String> consignorFreeText;
        if (shipmentConsigner.getIsAddressFreeText() != null && shipmentConsigner.getIsAddressFreeText()) {
            var rawData = consignerAddress != null && consignerAddress.containsKey(PartiesConstants.RAW_DATA) ? StringUtility.convertToString(consignerAddress.get(PartiesConstants.RAW_DATA)) : null;
            consignorFreeText = ReportHelper.getAddressList(rawData);
            dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consignorFreeText);
            dictionary.put(ReportConstants.CONSIGNER_ADDRESS_FREE_TEXT_IN_CAPS, consignorFreeText == null ? null : consignorFreeText.stream().map(StringUtility::toUpperCase).toList());
            dictionary.put(ReportConstants.CONSIGNER_NAME_FREETEXT_INCAPS, consignorFreeText == null ? null : consignorFreeText.stream().map(StringUtility::toUpperCase).toList());
        } else {
            dictionary.put(ReportConstants.CONSIGNER_FREETEXT, consigner);
        }
    }

    private void populateNotifyPartyData(Map<String, Object> dictionary, PartiesModel shipmentNotify, String email) {
        List<String> notify = null;
        List<String> notifyWoCont = null;
        if(shipmentNotify != null)
        {
            Map<String, Object> notifyAddress = shipmentNotify.getAddressData();
            if(notifyAddress != null)
            {
                notify = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(notifyAddress, COMPANY_NAME), getValueFromMap(notifyAddress, ADDRESS1),
                        getValueFromMap(notifyAddress, ADDRESS2),
                        ReportHelper.getCityCountry(getValueFromMap(notifyAddress, CITY), getValueFromMap(notifyAddress, COUNTRY)),
                        getValueFromMap(notifyAddress, email), getValueFromMap(notifyAddress, CONTACT_PHONE),
                        getValueFromMap(notifyAddress,ZIP_POST_CODE));

                notifyWoCont = ReportHelper.getOrgAddressWithoutPhoneEmail(
                        getValueFromMap(notifyAddress, COMPANY_NAME),
                        getValueFromMap(notifyAddress, ADDRESS1),
                        getValueFromMap(notifyAddress, ADDRESS2),
                        getValueFromMap(notifyAddress, CITY),
                        getValueFromMap(notifyAddress, STATE),
                        getValueFromMap(notifyAddress, ZIP_POST_CODE),
                        getValueFromMap(notifyAddress, COUNTRY)
                );

                dictionary.put(ReportConstants.NOTIFY_PARTY_NAME,getValueFromMap(notifyAddress, COMPANY_NAME));
                dictionary.put(ReportConstants.NOTIFY_PARTY_CONTACT_PERSON,getValueFromMap(notifyAddress, CONTACT_PERSON_ALIAS));
            }
            if(shipmentNotify.getOrgData() != null)
                dictionary.put(ReportConstants.NOTIFY_PARTY_LOCAL_NAME,getValueFromMap(shipmentNotify.getOrgData(), LOCAL_NAME));
            processNotifyPartyAddressFreeText(dictionary, shipmentNotify, notifyAddress, notify);

        }
        dictionary.put(ReportConstants.NOTIFY_PARTY, notify);
        dictionary.put(NOTIFY_PARTY_ADD_WITHOUT_CONTACT, notifyWoCont);
    }

    private void processNotifyPartyAddressFreeText(Map<String, Object> dictionary, PartiesModel shipmentNotify, Map<String, Object> notifyAddress, List<String> notify) {
        List<String> notifyPartyFreeText;
        if (shipmentNotify.getIsAddressFreeText() != null && shipmentNotify.getIsAddressFreeText()) {
            var rawData = notifyAddress != null && notifyAddress.containsKey(PartiesConstants.RAW_DATA) ? StringUtility.convertToString(notifyAddress.get(PartiesConstants.RAW_DATA)) : null;
            notifyPartyFreeText = ReportHelper.getAddressList(rawData);
            dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, notifyPartyFreeText);
            dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT_IN_CAPS, notifyPartyFreeText == null ? null : notifyPartyFreeText.stream().map(StringUtility::toUpperCase).toList());
            dictionary.put(ReportConstants.NOTIFY_PARTY_NAME_FREETEXT_INCAPS, notifyPartyFreeText == null || notifyPartyFreeText.isEmpty() ? null : StringUtility.toUpperCase(notifyPartyFreeText.get(0)));
        } else {
            dictionary.put(ReportConstants.NOTIFY_PARTY_FREETEXT, notify);
        }
    }

    private void populateClientData(Map<String, Object> dictionary, PartiesModel shipmentClient, String email) {
        List<String> client = null;
        List<String> clientWoCont = null;
        if(shipmentClient != null)
        {
            Map<String, Object> clientAddress = shipmentClient.getAddressData();
            if(clientAddress != null) {
                client = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(clientAddress, COMPANY_NAME), getValueFromMap(clientAddress, ADDRESS1),
                        getValueFromMap(clientAddress, ADDRESS2),
                        ReportHelper.getCityCountry(getValueFromMap(clientAddress, CITY), getValueFromMap(clientAddress, COUNTRY)),
                        getValueFromMap(clientAddress, email), getValueFromMap(clientAddress, CONTACT_PHONE),
                        getValueFromMap(clientAddress, ZIP_POST_CODE));

                clientWoCont = ReportHelper.getOrgAddressWithoutPhoneEmail(getValueFromMap(clientAddress, COMPANY_NAME), getValueFromMap(clientAddress, ADDRESS1),
                        getValueFromMap(clientAddress, ADDRESS2),
                        getValueFromMap(clientAddress, CITY), getValueFromMap(clientAddress, STATE), getValueFromMap(clientAddress, ZIP_POST_CODE),
                        getValueFromMap(clientAddress, COUNTRY));

                dictionary.put(ReportConstants.CLIENT_NAME, getValueFromMap(clientAddress, COMPANY_NAME));
                dictionary.put(ReportConstants.CLIENT_ADDRESS_1, getValueFromMap(clientAddress, ADDRESS1));
                dictionary.put(CLIENT_ADDRESS_COUNTRY, getValueFromMap(clientAddress, COUNTRY));
                dictionary.put(CLIENT_ADDRESS_CITY, getValueFromMap(clientAddress, CITY));
                dictionary.put(ReportConstants.CLIENT_ADDRESS_PHONE, getValueFromMap(clientAddress, CONTACT_PHONE));
                dictionary.put(ReportConstants.CLIENT_ADDRESS_MOBILE, getValueFromMap(clientAddress, "Mobile"));
                dictionary.put(ReportConstants.CLIENT_ADDRESS_CONTACT_PERSON, getValueFromMap(clientAddress, CONTACT_PERSON_ALIAS));
            }
        }
        dictionary.put(ReportConstants.CLIENT, client);
        dictionary.put(CLIENT_ADD_WITHOUT_CONTACT, clientWoCont);
    }

    private void addOtherTagsAndProcessMasterData(ShipmentModel shipment, Map<String, Object> dictionary, Map<Integer, Map<String, MasterData>> masterListsMap, V1TenantSettingsResponse v1TenantSettingsResponse, PickupDeliveryDetailsModel delivery, PickupDeliveryDetailsModel pickup, AdditionalDetailModel additionalDetails, UnlocationsResponse placeOfIssue, UnlocationsResponse paidPlace, UnlocationsResponse destination, String tsDateTimeFormat, UnlocationsResponse pol, UnlocationsResponse pod) {
        MasterData masterData = getMasterData(shipment, dictionary, masterListsMap);
        dictionary.put(ReportConstants.SHIPMENT_TYPE_DESCRIPTION, masterData != null ? masterData.getItemDescription() : shipment.getDirection());
        dictionary.put(ReportConstants.SHIPMENT_NUMBER, shipment.getShipmentId());
        dictionary.put(ReportConstants.FLIGHT_NUMBER, shipment.getCarrierDetails().getFlightNumber());
        dictionary.put(ReportConstants.ADDITIONAL_TERMS, shipment.getAdditionalTerms());

        masterData = getMasterDataForPackUnitAndAddTags(shipment, dictionary, masterListsMap, v1TenantSettingsResponse, masterData);

        if(shipment.getInnerPacks() != null)
            dictionary.put(ReportConstants.INNERS, getDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getInnerPacks()), 0, v1TenantSettingsResponse));

        dictionary.put(ReportConstants.PAYMENT_TERMS , shipment.getPaymentTerms() == null ?"" : shipment.getPaymentTerms());

        if (masterListsMap.containsKey(MasterDataType.PAYMENT.getId()) && masterListsMap.get(MasterDataType.PAYMENT.getId()).containsKey(shipment.getPaymentTerms()))
            masterData = masterListsMap.get(MasterDataType.PAYMENT.getId()).get(shipment.getPaymentTerms());
        if(masterData != null) {
            dictionary.put(ReportConstants.PAYMENT_TERMS_DESCRIPTION, masterData.getItemDescription());
        }

        if (masterListsMap.containsKey(MasterDataType.SERVICE_MODE.getId()) && masterListsMap.get(MasterDataType.SERVICE_MODE.getId()).containsKey(shipment.getServiceType())) {
            masterData = masterListsMap.get(MasterDataType.SERVICE_MODE.getId()).get(shipment.getServiceType());
            dictionary.put(ReportConstants.SERVICE_MODE_DESCRIPTION, StringUtility.isNotEmpty(masterData.getItemDescription()) ? StringUtility.toUpperCase(masterData.getItemDescription()) : shipment.getServiceType());
        }

        dictionary.put(ReportConstants.GROSS_WEIGHT, convertToWeightNumberFormat(shipment.getWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.GROSS_WEIGHT_UNIT, shipment.getWeightUnit());
        dictionary.put(ReportConstants.GROSS_VOLUME, convertToVolumeNumberFormat(shipment.getVolume(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.GROSS_VOLUME_UNIT, shipment.getVolumeUnit());
        dictionary.put(ReportConstants.GROSS_WEIGHT_WITH_COMMA, convertToWeightNumberFormat(shipment.getWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.GROSS_VOLUME_WITH_COMMA, convertToVolumeNumberFormat(shipment.getVolume(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.VOLUME_WEIGHT_WITH_COMMA, addCommas(shipment.getVolumetricWeight()));
        dictionary.put(ReportConstants.WEIGHT_UNIT_DESCRIPTION,  shipment.getNetWeightUnit());
        dictionary.put(ReportConstants.CHARGEABLE_WEIGHT, convertToVolumeNumberFormat(shipment.getChargable(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CHARGEABLE_WEIGHT_UNIT, shipment.getChargeableUnit());
        dictionary.put(ReportConstants.WEIGHTS, convertToWeightNumberFormat(shipment.getNetWeight(), v1TenantSettingsResponse));
        addWeigthVolUnitTags(shipment, dictionary, masterListsMap, v1TenantSettingsResponse);

        addTagsWithDefaultValues(dictionary, delivery, pickup, additionalDetails, placeOfIssue);
        dictionary.put(ReportConstants.MARKS_N_NUMS, shipment.getMarksNum());
        addPaidPlaceTags(dictionary, paidPlace);

        dictionary.put(ReportConstants.HSN_NUMBER, additionalDetails.getHsnNumber());
        dictionary.put(ReportConstants.SHIPMENT_BOOKING_NUMBER, shipment.getBookingNumber());

        processDestinationTags(dictionary, masterListsMap, v1TenantSettingsResponse, destination, tsDateTimeFormat);

        addAtTag(shipment, dictionary, pol, pod);
        addPaymentTermsTag(shipment, dictionary, masterListsMap);
        dictionary.put(ReportConstants.MARKS_N_NUMS_CAPS, StringUtility.toUpperCase(shipment.getMarksNum()));


        var array = new String[] {"" + dictionary.get("VesselName"), shipment.getCarrierDetails().getVoyage()};
        dictionary.put(ReportConstants.VESSEL_NAME_AND_VOYAGE, array[0] + " & " + array[1]);

        masterData = null;
        if(placeOfIssue != null && masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(placeOfIssue.getCountry()))  {
            masterData = masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(placeOfIssue.getCountry());
        }
        dictionary.put(ReportConstants.ISSUEPLACECOUNTRYNAME, masterData != null ? masterData.getItemDescription() : null);
    }

    private void addTagsWithDefaultValues(Map<String, Object> dictionary, PickupDeliveryDetailsModel delivery, PickupDeliveryDetailsModel pickup, AdditionalDetailModel additionalDetails, UnlocationsResponse placeOfIssue) {
        dictionary.put(ReportConstants.DELIVERY_CFS, (delivery != null && !Objects.isNull(delivery.getSourceDetail()) && !Objects.isNull(delivery.getSourceDetail().getOrgData())) ? delivery.getSourceDetail().getOrgData().get(FULL_NAME) : null);
        dictionary.put(ReportConstants.PICKUP_CFS, (pickup != null && !Objects.isNull(pickup.getDestinationDetail()) && !Objects.isNull(pickup.getDestinationDetail().getOrgData()) )? pickup.getDestinationDetail().getOrgData().get(FULL_NAME) : null);

        dictionary.put(ReportConstants.ORIGINALS, additionalDetails.getOriginal() == null ? 1 : additionalDetails.getOriginal());
        dictionary.put(ReportConstants.ORIGINAL_WORDS, numberToWords(additionalDetails.getOriginal() == null ? 1 : additionalDetails.getOriginal()));
        dictionary.put(ReportConstants.COPY_BILLS, additionalDetails.getCopy() == null ? 0 : additionalDetails.getCopy());

        dictionary.put(ReportConstants.ISSUE_PLACE_NAME, placeOfIssue !=  null ? populateNameFromUnlocationsResponse(placeOfIssue) : null);
        dictionary.put(ReportConstants.ISSUE_PLACE_COUNTRY, placeOfIssue != null ? placeOfIssue.getCountry() : null);
    }

    private void addPaymentTermsTag(ShipmentModel shipment, Map<String, Object> dictionary, Map<Integer, Map<String, MasterData>> masterListsMap) {
        dictionary.put(PAYMENT_TERMS_DESCRITION_WITH_CAMELCASE,  shipment.getPaymentTerms());
        if (shipment.getPaymentTerms() != null && masterListsMap.containsKey(MasterDataType.PAYMENT.getId()) && masterListsMap.get(MasterDataType.PAYMENT.getId()).containsKey(shipment.getPaymentTerms())) {
            dictionary.put(PAYMENT_TERMS_DESCRITION_WITH_CAMELCASE,  StringUtility.toUpperCase(masterListsMap.get(MasterDataType.PAYMENT.getId()).get(shipment.getPaymentTerms()).getItemDescription()));
        }
    }

    private void addPaidPlaceTags(Map<String, Object> dictionary, UnlocationsResponse paidPlace) {
        dictionary.put(ReportConstants.PAID_PLACE_NAME, paidPlace != null ? populateNameFromUnlocationsResponse(paidPlace) : null);
        dictionary.put(ReportConstants.PAID_PLACE_COUNTRY, paidPlace != null ? paidPlace.getCountry() : null);

        dictionary.put(ReportConstants.PAID_PLACE_NAME_IN_CAPS, paidPlace != null ? StringUtility.toUpperCase(paidPlace.getName()) : null);
        dictionary.put(ReportConstants.PAID_PLACE_COUNTRY_IN_CAPS, paidPlace != null ? StringUtility.toUpperCase(paidPlace.getCountry()) : null);
    }

    private String populateNameFromUnlocationsResponse(UnlocationsResponse unlocationsResponse) {

        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsRunnerV3Enabled())) {
            if (unlocationsResponse.getLocCode().length() >= 2) {
                // Extract country code from UNLOC using initial 2 characters
                String countryCode = unlocationsResponse.getLocCode().substring(0, 2);
                if (US.equalsIgnoreCase(countryCode)) {
                    return (countryCode + "," + unlocationsResponse.getName()).toUpperCase();
                }
            }
            return unlocationsResponse.getName().toUpperCase();
        }
        return unlocationsResponse.getName();
    }

    private MasterData getMasterDataForPackUnitAndAddTags(ShipmentModel shipment, Map<String, Object> dictionary, Map<Integer, Map<String, MasterData>> masterListsMap, V1TenantSettingsResponse v1TenantSettingsResponse, MasterData masterData) {
        dictionary.put(ReportConstants.PACKS, getDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getNoOfPacks() != null ? shipment.getNoOfPacks() : 0), 0, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.PACKS_UNIT,Constants.MPK.equals(shipment.getPacksUnit()) ? Constants.PIECES : shipment.getPacksUnit());
        dictionary.put(ReportConstants.PACKS_WITH_COMMA, addCommas(shipment.getNoOfPacks()));
        if (masterListsMap.containsKey(MasterDataType.PACKS_UNIT.getId()) && masterListsMap.get(MasterDataType.PACKS_UNIT.getId()).containsKey(shipment.getPacksUnit()))
            masterData = masterListsMap.get(MasterDataType.PACKS_UNIT.getId()).get(shipment.getPacksUnit());
        String packsUnit = masterData != null && StringUtility.isNotEmpty(masterData.getItemDescription()) ? masterData.getItemDescription() : shipment.getPacksUnit();
        dictionary.put(ReportConstants.PACKS_UNIT_DESC, Constants.MULTI_PACK.equals(packsUnit) ? Constants.PACKAGES : packsUnit);
        return masterData;
    }

    private MasterData getMasterData(ShipmentModel shipment, Map<String, Object> dictionary, Map<Integer, Map<String, MasterData>> masterListsMap) {
        MasterData masterData = null;
        if (masterListsMap.containsKey(MasterDataType.TRANSPORT_MODE.getId()) && masterListsMap.get(MasterDataType.TRANSPORT_MODE.getId()).containsKey(shipment.getTransportMode()))
            masterData = masterListsMap.get(MasterDataType.TRANSPORT_MODE.getId()).get(shipment.getTransportMode());
        dictionary.put(ReportConstants.TRANSPORT_MODE_DESCRIPTION, masterData != null ? masterData.getItemDescription() : shipment.getTransportMode());
        if (masterListsMap.containsKey(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId()) && masterListsMap.get(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId()).containsKey(shipment.getDirection()))
            masterData = masterListsMap.get(MasterDataType.CUSTOM_SHIPMENT_TYPE.getId()).get(shipment.getDirection());
        return masterData;
    }

    private void addAtTag(ShipmentModel shipment, Map<String, Object> dictionary, UnlocationsResponse pol, UnlocationsResponse pod) {
        if (Objects.equals(shipment.getPaymentTerms(), "PPD")) {
            dictionary.put(ReportConstants.AT, pol != null ? pol.getName() : null);
        } else if (Objects.equals(shipment.getPaymentTerms(), "CCX")) {
            dictionary.put(ReportConstants.AT, pod != null ? pod.getName() : null);
        }
    }

    private void addWeigthVolUnitTags(ShipmentModel shipment, Map<String, Object> dictionary, Map<Integer, Map<String, MasterData>> masterListsMap, V1TenantSettingsResponse v1TenantSettingsResponse) {
        MasterData masterData;
        dictionary.put(WEIGHT, convertToWeightNumberFormat(shipment.getWeight(), v1TenantSettingsResponse));
        dictionary.put(VOLUME, convertToVolumeNumberFormat(shipment.getVolume(), v1TenantSettingsResponse));
        if (shipment.getVolumeUnit() != null && masterListsMap.containsKey(MasterDataType.VOLUME_UNIT.getId())) {
            masterData =  masterListsMap.get(MasterDataType.VOLUME_UNIT.getId()).get(shipment.getVolumeUnit());
            dictionary.put(VOLUME_UNIT_DESCRIPTION, masterData != null && masterData.getItemDescription() != null ? StringUtility.toUpperCase(masterData.getItemDescription()) : shipment.getVolumeUnit());
        }
        dictionary.put(NET_WEIGHT, addCommas(shipment.getNetWeight()));
        dictionary.put(NET_WEIGHT_UNIT_DESCRIPTION, shipment.getNetWeightUnit());
        if (shipment.getNetWeightUnit() != null && masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT.getId()) && masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).containsKey(shipment.getNetWeightUnit())) {
            masterData = masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).get(shipment.getNetWeightUnit());
            dictionary.put(NET_WEIGHT_UNIT_DESCRIPTION, (masterData != null && masterData.getItemDescription() != null) ? StringUtility.toUpperCase(masterData.getItemDescription()) : shipment.getVolumeUnit());
        }
    }

    private void processDestinationTags(Map<String, Object> dictionary, Map<Integer, Map<String, MasterData>> masterListsMap, V1TenantSettingsResponse v1TenantSettingsResponse, UnlocationsResponse destination, String tsDateTimeFormat) {
        dictionary.put(ReportConstants.DESTINATION_NAME, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.DESTINATION, destination != null ? destination.getName() : null);
        dictionary.put(ReportConstants.DESTINATION_COUNTRY, destination != null ? destination.getCountry() : null);

        dictionary.put(ReportConstants.PRINT_DATE, convertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat, v1TenantSettingsResponse));
        if(destination != null) {
            dictionary.put(ReportConstants.DESTINATION_NAME_IN_CAPS, StringUtility.toUpperCase(destination.getName()));
            String destinationCountry = masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(destination.getCountry()) ? masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(destination.getCountry()).getItemDescription() : "";
            dictionary.put(ReportConstants.DESTINATION_COUNTRY_NAME_IN_CAPS, destinationCountry.toUpperCase());
            dictionary.put(DESTINATION_COUNTRY_NAME, destinationCountry);
            dictionary.put(ReportConstants.FPOD_IN_CAPS, StringUtility.toUpperCase(destination.getName()));
            dictionary.put(ReportConstants.FPOD_COUNTRY_NAME_IN_CAPS, StringUtility.toUpperCase(destinationCountry));
            dictionary.put(DESTINATION_CODE_IN_CAPS, StringUtility.toUpperCase(destination.getLocCode()));
        }
    }

    private int getContainerCount(ShipmentModel shipment) {
        int containerCount = 0;
        if (!shipment.getContainersList().isEmpty()) {
            for (ContainerModel container : shipment.getContainersList()) {
                if (container.getContainerCount() != null && container.getContainerCount() != 0) {
                    containerCount += container.getContainerCount().intValue();
                }
            }
        }
        return containerCount;
    }

    private void processPolPodTags(ShipmentModel shipment, Map<String, Object> dictionary, UnlocationsResponse pol, UnlocationsResponse pod, AdditionalDetailModel additionalDetails, Map<Integer, Map<String, MasterData>> masterListsMap) {
        addPolAndPodCodeTags(shipment, dictionary, pol, pod);
        dictionary.put(ReportConstants.POD_COUNTRY, pod != null ? pod.getCountry() : null);
        dictionary.put(ReportConstants.POL_COUNTRY, pol != null ? pol.getCountry() : null);
        dictionary.put(CARGO_TERMS_DESCRIPTION, StringUtility.toUpperCase(shipment.getGoodsDescription()));
        dictionary.put(BL_DESCRIPTION, StringUtility.toUpperCase(additionalDetails.getBLRemarksDescription()));
        dictionary.put(LOAD_DESCRIPTION_REMARKS, StringUtility.toUpperCase(additionalDetails.getBLRemarks()));
        processPolTags(dictionary, pol, masterListsMap);
        if(shipment.getAdditionalDetails() != null) {
            dictionary.put(ReportConstants.COUNTRY_OF_GOODS_ORIGIN, shipment.getAdditionalDetails().getGoodsCO());
        }
        dictionary.put(CONTAINER_SUMMARY, shipment.getSummary());
        dictionary.put(PACK_SUMMARY, shipment.getPackSummary());
        dictionary.put(ReportConstants.SERVICE_MODE, shipment.getServiceType());
        dictionary.put(ReportConstants.POL_PORTNAME, pol != null ? pol.getPortName() : null);
        dictionary.put(ReportConstants.POL_PORT_NAME_IN_CAPS, pol != null ? StringUtility.toUpperCase(pol.getPortName()) : null);
        dictionary.put(ReportConstants.POD_PORTNAME, pod != null ? pod.getPortName() : null);
        dictionary.put(ReportConstants.POD_PORT_NAME_IN_CAPS, pod != null ? StringUtility.toUpperCase(pod.getPortName()) : null);

        processPodTags(dictionary, pod, masterListsMap);

        dictionary.put(ReportConstants.CARRIER, shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getShippingLine() : null);
        dictionary.put(ReportConstants.PORT_OF_DISCHARGE, pod != null ? pod.getName() : null);
        dictionary.put(ReportConstants.PORT_OF_LOADING, pol != null ? pol.getName() : null);
    }

    private void processPolTags(Map<String, Object> dictionary, UnlocationsResponse pol, Map<Integer, Map<String, MasterData>> masterListsMap) {
        if(pol != null) {
            if(pol.getPortName() != null) {
                dictionary.put(ReportConstants.POL_PORT_NAME_WITH_COUNTRY_IN_CAPS, (pol.getPortName().toUpperCase() + ", " + (masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(pol.getCountry()) ? masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(pol.getCountry()).getItemDescription().toUpperCase() : "")));
                dictionary.put(ReportConstants.POL_IN_CAPS, pol.getPortName().toUpperCase());
                dictionary.put(ORIGIN_PORT, dictionary.get(POL_IN_CAPS));
            }
            else
                dictionary.put(ORIGIN_PORT, pol.getName());
            dictionary.put(POL_COUNTRY_NAME, pol.getCountryName());
            dictionary.put(POL_COUNTRY_NAME_IN_CAPS , (masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(pol.getCountry()) ? masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(pol.getCountry()).getItemDescription().toUpperCase() : ""));
            dictionary.put(ReportConstants.POL_AIRPORT_CODE, pol.getIataCode());
            if(pol.getIataCode() != null) {
                dictionary.put(ReportConstants.POL_AIRPORT_CODE_IN_CAPS, pol.getIataCode().toUpperCase());
            }
            dictionary.put(POL_CODE_IN_CAPS, StringUtility.toUpperCase(pol.getLocCode()));
        }
    }

    private void processPodTags(Map<String, Object> dictionary, UnlocationsResponse pod, Map<Integer, Map<String, MasterData>> masterListsMap) {
        if (pod != null) {
            String podCountry = masterListsMap.containsKey(MasterDataType.COUNTRIES.getId()) && masterListsMap.get(MasterDataType.COUNTRIES.getId()).containsKey(pod.getCountry()) ? masterListsMap.get(MasterDataType.COUNTRIES.getId()).get(pod.getCountry()).getItemDescription() : "";
            dictionary.put(ReportConstants.DESTINATION_AIRPORT_COUNTRY, podCountry.toUpperCase());
            if (pod.getPortName() != null) {
                dictionary.put(ReportConstants.POD_PORT_NAME_WITH_COUNTRY_IN_CAPS, pod.getPortName().toUpperCase() + ", " + podCountry.toUpperCase());
                dictionary.put(ReportConstants.POD_IN_CAPS, pod.getPortName().toUpperCase());
                dictionary.put(DESTINATION_PORT, dictionary.get(POD_IN_CAPS));
            }
            else
                dictionary.put(DESTINATION_PORT, pod.getName());
            dictionary.put(POD_COUNTRY_NAME, pod.getCountryName());
            dictionary.put(ReportConstants.POD_COUNTRY_NAME_IN_CAPS, podCountry.toUpperCase());
            dictionary.put(ReportConstants.POD_AIRPORT_CODE, pod.getIataCode());
            if (pod.getIataCode() != null) {
                dictionary.put(ReportConstants.POD_AIRPORT_CODE_IN_CAPS, pod.getIataCode().toUpperCase());
            }
            if (pod.getPortName() != null)
                dictionary.put(ReportConstants.POD_IN_CAPS, pod.getPortName().toUpperCase());
            dictionary.put(POD_CODE_IN_CAPS, StringUtility.toUpperCase(pod.getLocCode()));
        }
    }

    private void addPolAndPodCodeTags(ShipmentModel shipment, Map<String, Object> dictionary, UnlocationsResponse pol, UnlocationsResponse pod) {
        if(!Objects.isNull(pol)) {
            dictionary.put(ReportConstants.POL_CODE, pol.getLocCode());
            if (Objects.equals(shipment.getTransportMode(), AIR) && BOOKING_ORDER.equalsIgnoreCase(shipment.getDocument()))
                dictionary.put(ReportConstants.POL_CODE, pol.getIataCode());
        }
        if(!Objects.isNull(pod)) {
            dictionary.put(ReportConstants.POD_CODE, pod.getLocCode());
            if (Objects.equals(shipment.getTransportMode(), AIR) && BOOKING_ORDER.equalsIgnoreCase(shipment.getDocument()))
                dictionary.put(ReportConstants.POD_CODE, pod.getIataCode());
        }
    }

    public void processVessels(ShipmentModel shipment, Map<String, Object> dictionary, Set<String> vesselIds, String carrierVesselId) {
        vesselIds.add(carrierVesselId);
        Map<String, EntityTransferVessels> entityTransferVesselsMap = masterDataUtils.getVesselDataFromCache(vesselIds);
        Map<String, VesselsResponse> vesselsResponseMap = new HashMap<>();
        for (Map.Entry<String, EntityTransferVessels> entry : entityTransferVesselsMap.entrySet()) {
            String key = entry.getKey();
            VesselsResponse value = jsonHelper.convertValue(entry.getValue(), VesselsResponse.class);
            vesselsResponseMap.put(key, value);
        }
        VesselsResponse vesselsResponse = vesselsResponseMap.get(carrierVesselId);
        if(Objects.equals(shipment.getTransportMode(), AIR))
        {
            dictionary.put(VESSELS_NAME_FLIGHT_NAME, shipment.getCarrierDetails() != null ? shipment.getCarrierDetails().getShippingLine() : null);
        }
        if(vesselsResponse != null) {
            dictionary.put(VESSEL_NAME, vesselsResponse.getName());
            if(!Objects.equals(shipment.getTransportMode(), AIR))
                dictionary.put(VESSELS_NAME_FLIGHT_NAME, vesselsResponse.getName());
        }
    }

    private Map<Integer, Map<String, MasterData>> getMasterListsMap(List<MasterListRequest> masterListRequest) {
        Map<String, EntityTransferMasterLists> entityTransferMasterListsMap = masterDataUtils.fetchMasterListFromCache(MasterListRequestV2.builder().MasterListRequests(masterListRequest.stream().filter(Objects::nonNull).collect(Collectors.toList())).build());
        Map<Integer, Map<String, MasterData>> masterListsMap = new HashMap<>();
        for (Map.Entry<String, EntityTransferMasterLists> entry : entityTransferMasterListsMap.entrySet()) {
            String key = entry.getKey();
            String[] parts = key.split("#");
            String itemType = parts[0];
            String itemValue = parts[1];
            MasterDataType masterDataType = MasterDataType.valueOf(itemType);
            int masterDataKey = masterDataType.getId();
            MasterData masterData = jsonHelper.convertValue(entry.getValue(), MasterData.class);
            masterListsMap.computeIfAbsent(masterDataKey, k -> new HashMap<>()).put(itemValue, masterData);
        }
        return masterListsMap;
    }

    public Map<String, UnlocationsResponse> getUnlocationsResponseMap(ShipmentModel shipment) {
        List<String> unlocoRequests = this.createUnLocoRequestFromShipmentModel(shipment);
        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        Map<String, EntityTransferUnLocations> entityTransferUnLocationsMap = masterDataUtils.getLocationDataFromCache(new HashSet<>(unlocoRequests), EntityTransferConstants.LOCATION_SERVICE_GUID);
        for (Map.Entry<String, EntityTransferUnLocations> entry : entityTransferUnLocationsMap.entrySet()) {
            String key = entry.getKey();
            UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
            unlocationsMap.put(key, value);
        }
        return unlocationsMap;
    }

    private void populateShipmentCargoManifestParty(ShipmentModel shipmentModel, Map<String, Object> dictionary) {
        // Consigner
        var shipmentConsigner = shipmentModel.getConsigner();
        ReportHelper.populateCargoManifestPartyAddress(dictionary, shipmentConsigner, CM_CONSIGNER);

        // Consignee
        var shipmentConsignee = shipmentModel.getConsignee();
        ReportHelper.populateCargoManifestPartyAddress(dictionary, shipmentConsignee, CM_CONSIGNEE);

        AdditionalDetailModel additionalDetailModel = Optional.ofNullable(shipmentModel.getAdditionalDetails()).orElse(new AdditionalDetailModel());

        // Origin Agent
        var shipmentOriginAgent = additionalDetailModel.getExportBroker();
        ReportHelper.populateCargoManifestPartyAddress(dictionary, shipmentOriginAgent, CM_ORIGIN_AGENT_ADDRESS);

        // Destination Agent
        var shipmentDestinationAgent = additionalDetailModel.getImportBroker();
        ReportHelper.populateCargoManifestPartyAddress(dictionary, shipmentDestinationAgent, CM_DESTINATION_AGENT_ADDRESS);

    }

    public void populateShipmentOrders(ShipmentModel shipment, Map<String, Object> dictionary) {
        if(ObjectUtils.isNotEmpty(shipment.getShipmentOrders())){
            var orderNumbers = shipment.getShipmentOrders().stream()
                    .map(ShipmentOrderModel::getOrderNumber)
                    .collect(Collectors.joining(","));
            dictionary.put(ORDER_MANAGEMENT_NUMBER, orderNumbers);
        }
    }

    private String getConcatenatedContact(String code, String number) {
        return Objects.toString(code, "") + " " + Objects.toString(number, "");
    }

    public Map<String, Object> populateHAWBAndSecurityData(List<ShipmentModel> shipmentModelList, List<Awb> awbList, Map<String, Object> dictionary, boolean isSecurity, boolean isShipperAndConsignee, boolean fromConsolidation) {
        List<Object> shipAwbDataList = new ArrayList<>();
        if(shipmentModelList == null)
            shipmentModelList = new ArrayList<>();
        if(awbList == null)
            awbList = new ArrayList<>();
        for (int i=0; i < shipmentModelList.size(); i++) {
            Awb awb = null;
            if(awbList.size() >= i + 1)
                awb = awbList.get(i);
            ShipmentModel shipmentModel = shipmentModelList.get(i);
            Map<String, Object> dict = new HashMap<>();
            if(dictionary == null || fromConsolidation)
                populateShipmentFields(shipmentModel, dict);
            else
                dict = new HashMap<>(dictionary);
            getPackingDetails(shipmentModel, dict);
            dict.put(VOLUME_WEIGHT, convertToWeightNumberFormat(shipmentModel.getVolumetricWeight()));
            dict.put(VOLUME_WEIGHT_UNIT, shipmentModel.getVolumetricWeightUnit());
            dict.put(IS_SECURITY, Boolean.TRUE.equals(isSecurity));
            if(dictionary == null)
                dictionary = new HashMap<>();
            dictionary.put(SCI, shipmentModel.getAdditionalDetails().getSci());
            populateRaKcData(dict, shipmentModel);
            populateAwbDetails(dictionary, awb, dict);
            dict.put(WITH_CONSIGNOR, isShipperAndConsignee);
            populateDirectionTags(shipmentModel, dict);
            processReferenceNumbersListTag(shipmentModel, dict);
            if(shipmentModel.getAdditionalDetails() != null) {
                dict.put(NOTIFY_PARTY, ReportHelper.getOrgAddressDetails(shipmentModel.getAdditionalDetails().getNotifyParty()));
            }
            String chargeableString = getDPWWeightVolumeFormat(shipmentModel.getChargable(), CHARGEABLE_WEIGHT_DECIMAL_PLACES, getCurrentTenantSettings(), true);
            dict.put(CHARGEABLE, chargeableString);
            dict.put(CHARGABLE_AND_UNIT, String.format(REGEX_S_S, chargeableString, shipmentModel.getChargeableUnit()));
            dict.put(CHARGEABLE_AND_UNIT, dict.get(CHARGABLE_AND_UNIT));
            shipAwbDataList.add(dict);
        }
        if(dictionary == null)
            dictionary = new HashMap<>();
        dictionary.put(SHIPMENT, shipAwbDataList);
        return dictionary;
    }

    private void processReferenceNumbersListTag(ShipmentModel shipmentModel, Map<String, Object> dict) {
        if(shipmentModel.getReferenceNumbersList() != null && !shipmentModel.getReferenceNumbersList().isEmpty()) {
            for (ReferenceNumbersModel referenceNumbersModel: shipmentModel.getReferenceNumbersList()) {
                if(Objects.equals(referenceNumbersModel.getType(), ReportConstants.MRN))
                    dict.computeIfAbsent(ReportConstants.MO_RN, k -> referenceNumbersModel.getReferenceNumber());
            }
        }
    }

    private void populateDirectionTags(ShipmentModel shipmentModel, Map<String, Object> dict) {
        if(shipmentModel.getDirection().equals(IMP)) {
            try {
                dict.put(ORIGIN_AGENT_RN_NUMBER, shipmentModel.getAdditionalDetails().getImportBroker().getAddressData().get(KCRA_NUMBER));} catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
            try {
                dict.put(DESTINATION_AGENT_RN_NUMBER, shipmentModel.getAdditionalDetails().getExportBroker().getAddressData().get(KCRA_NUMBER));} catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        }
        else {
            try {
                dict.put(ORIGIN_AGENT_RN_NUMBER, shipmentModel.getAdditionalDetails().getExportBroker().getAddressData().get(KCRA_NUMBER));} catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
            try {
                dict.put(DESTINATION_AGENT_RN_NUMBER, shipmentModel.getAdditionalDetails().getImportBroker().getAddressData().get(KCRA_NUMBER));} catch (Exception ignored) {log.error(ORG_DATA_NOT_AVAILABLE);}
        }
    }

    private void populateAwbDetails(Map<String, Object> dictionary, Awb awb, Map<String, Object> dict) {
        if(awb != null) {
            if(awb.getAwbSpecialHandlingCodesMappings() != null && !awb.getAwbSpecialHandlingCodesMappings().isEmpty())
                dict.put(SPH, awb.getAwbSpecialHandlingCodesMappings().stream().map(AwbSpecialHandlingCodesMappingInfo::getShcId).collect(Collectors.toSet()));
            if(awb.getAwbCargoInfo() != null) {
                var cargoInfoRows = awb.getAwbCargoInfo();
                dict.put(SCI, cargoInfoRows.getSci());
                dictionary.put(RA_CSD, geteCSDInfo(awb));
                dict.put(RA_CSD, geteCSDInfo(awb));

                dict.put(ORIGINAL_PRINT_DATE, getPrintOriginalDate(awb));
                dictionary.put(ORIGINAL_PRINT_DATE, getPrintOriginalDate(awb));
                dict.put(USER_INITIALS, AwbUtility.getScreenersName(awb));
                dictionary.put(USER_INITIALS, AwbUtility.getScreenersName(awb));
            }
            populateAwbPartiesInfo(dict, awb);
            if(!listIsNullOrEmpty(awb.getAwbGoodsDescriptionInfo())) {
                List<String> chargeableWt = new ArrayList<>();
                for(AwbGoodsDescriptionInfo awbGoodsDescriptionInfo : awb.getAwbGoodsDescriptionInfo()) {
                    if(awbGoodsDescriptionInfo.getChargeableWt() != null) {
                        chargeableWt.add(roundUpToNextHalf(awbGoodsDescriptionInfo.getChargeableWt().toString()));
                    }
                }
                dict.put(CHARGEABLE_WT, chargeableWt);
            }
        }
    }

    public void populateFreightsAndCharges(Map<String, Object> dictionary, Hbl hbl) {

        if (Objects.nonNull(hbl) && Objects.nonNull(hbl.getShipmentId())) {
            Optional<ShipmentDetails> shipmentDetails = shipmentDao.findById(hbl.getShipmentId());

            if (!shipmentDetails.isPresent() || Objects.isNull(shipmentDetails.get().getAdditionalDetails()))
                return;
            if (Boolean.TRUE.equals(shipmentDetails.get().getAdditionalDetails().getIsRatedBL())) {
                List<HblFreightsAndCharges> hblFreightsAndCharges = hbl.getHblFreightsAndCharges();

                // Validate First Row of freight and charges is mandate if Rated BL is true
                populateFreightsAndChargesValidation(hblFreightsAndCharges);
            } else {
                dictionary.put(BL_IS_NOT_RATED, BL_IS_NOT_RATED_VALUE);
            }

            // Process Freight and Charges and add List into Dictionary
            List<HblFreightsAndCharges> hblFreightsAndCharges = hbl.getHblFreightsAndCharges();
            if (Objects.nonNull(hblFreightsAndCharges) && !hblFreightsAndCharges.isEmpty()) {
                processFreightsAndCharges(hblFreightsAndCharges, dictionary);
            }
        }
    }

    private void processFreightsAndCharges(List<HblFreightsAndCharges> hblFreightsAndCharges,
                                           Map<String, Object> dictionary) {
        List<Map<String, Object>> freightChargesList = new ArrayList<>();

        for (HblFreightsAndCharges freightAndCharges : hblFreightsAndCharges) {
            Map<String, Object> currFreightAndCharges = new HashMap<>();
            currFreightAndCharges.put(CHARGES, freightAndCharges.getCharges());
            currFreightAndCharges.put(VALUE, freightAndCharges.getValue());
            currFreightAndCharges.put(CURRENCY, freightAndCharges.getCurrency());
            currFreightAndCharges.put(CHARGE_TYPE, freightAndCharges.getChargeType());

            freightChargesList.add(currFreightAndCharges);
        }

        dictionary.put(BL_CHARGES, freightChargesList);
    }


    private void populateFreightsAndChargesValidation(List<HblFreightsAndCharges> hblFreightsAndCharges) {
        if (Objects.isNull(hblFreightsAndCharges) || hblFreightsAndCharges.isEmpty()) {
            throw new ValidationException("At least one Freight & Charges row is mandatory when Rated BL is true.");
        }

        HblFreightsAndCharges firstFreightAndCharges = hblFreightsAndCharges.get(0);

        if (Objects.isNull(firstFreightAndCharges.getCharges()) ||
                Objects.isNull(firstFreightAndCharges.getValue()) ||
                Objects.isNull(firstFreightAndCharges.getCurrency()) ||
                Objects.isNull(firstFreightAndCharges.getChargeType())) {

            throw new ValidationException("The first Freight & Charges row must have Charges, Value, Currency, and Type filled.");
        }
    }

    public void populateAwbPartiesInfo(Map<String, Object> dictionary, Awb awb) {
        if(awb == null)
            return;
        if(awb.getAwbShipmentInfo() != null) {
            AwbShipmentInfo awbShipmentInfo = awb.getAwbShipmentInfo();
            dictionary.put(CM_SHIPPER_AWB_ADDRESS, formattedOrgAddress(awbShipmentInfo.getShipperName(), awbShipmentInfo.getShipperAddress(), awbShipmentInfo.getShipperAddress2(), awbShipmentInfo.getShipperCity(), awbShipmentInfo.getShipperState(), awbShipmentInfo.getShipperZipCode(), awbShipmentInfo.getShipperCountryName()));
            dictionary.put(CM_CONSIGNEE_AWB_ADDRESS, formattedOrgAddress(awbShipmentInfo.getConsigneeName(), awbShipmentInfo.getConsigneeAddress(), awbShipmentInfo.getConsigneeAddress2(), awbShipmentInfo.getConsigneeCity(), awbShipmentInfo.getConsigneeState(), awbShipmentInfo.getConsigneeZipCode(), awbShipmentInfo.getConsigneeCountryName()));
            if(!listIsNullOrEmpty(awb.getAwbNotifyPartyInfo())) {
                AwbNotifyPartyInfo awbNotifyPartyInfo = awb.getAwbNotifyPartyInfo().get(0);
                dictionary.put(CM_NOTIFY_AWB_ADDRESS, formattedOrgAddress(awbNotifyPartyInfo.getName(), awbNotifyPartyInfo.getAddress(), awbNotifyPartyInfo.getAddress2(), awbNotifyPartyInfo.getCity(), awbNotifyPartyInfo.getState(), awbNotifyPartyInfo.getZipCode(), awbNotifyPartyInfo.getCountryName()));
            }
        }
    }

    public boolean getAirRoutingFlightTags(List<RoutingsModel> routingsModels, Map<String, Object> dictionary, boolean fromShipment) {
        if(routingsModels == null)
            return false;
        TreeMap<Long, RoutingsModel> map = routingsModels.stream()
                .filter(e -> Constants.TRANSPORT_MODE_AIR.equals(e.getMode()))
                .filter(e -> RoutingCarriage.MAIN_CARRIAGE.equals(e.getCarriage()))
                .collect(Collectors.toMap(
                        RoutingsModel::getLeg,
                        e -> e,
                        (existing, replacement) -> existing,
                        TreeMap::new
                ));
        if(map.isEmpty())
            return false;
        Set<String> carriers = new HashSet<>();
        RoutingsModel firstRouting = map.firstEntry().getValue();
        RoutingsModel secondRouting = null;
        if(!CommonUtils.isStringNullOrEmpty(firstRouting.getCarrier()))
            carriers.add(firstRouting.getCarrier());
        Iterator<Map.Entry<Long, RoutingsModel>> iterator = map.entrySet().iterator();
        iterator.next();
        if(iterator.hasNext()) {
            secondRouting = iterator.next().getValue();
            if(!CommonUtils.isStringNullOrEmpty(secondRouting.getCarrier()))
                carriers.add(secondRouting.getCarrier());
        }
        Map<String, CarrierMasterData> carriersMap = masterDataUtils.getCarriersData(carriers);
        if(fromShipment) {
            dictionary.put(SHIPMENT_FIRST_FLIGHT_AND_DAY, getRoutingFlightAndDay(firstRouting, carriersMap));
            if(secondRouting != null)
                dictionary.put(SHIPMENT_SECOND_FLIGHT_AND_DAY, getRoutingFlightAndDay(secondRouting, carriersMap));
        } else {
            dictionary.put(CONSOL_FIRST_FLIGHT_AND_DAY, getRoutingFlightAndDay(firstRouting, carriersMap));
            if(secondRouting != null)
                dictionary.put(CONSOL_SECOND_FLIGHT_AND_DAY, getRoutingFlightAndDay(secondRouting, carriersMap));
        }
        return true;
    }

    public String getRoutingFlightAndDay(RoutingsModel routingsModel, Map<String, CarrierMasterData> map) {
        String res = "";
        if(routingsModel == null)
            return res;
        String flightNumber = Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getEnableRouteMaster()) ? routingsModel.getVoyage() : routingsModel.getFlightNumber();
        return getFlightAndDayString(map, routingsModel.getCarrier(), flightNumber, routingsModel.getEtd());
    }

    public String getFlightAndDayString(Map<String, CarrierMasterData> carriersMap, String carrierCode, String flightNumber, LocalDateTime etd) {
        if(!CommonUtils.isStringNullOrEmpty(carrierCode) && carriersMap != null && carriersMap.containsKey(carrierCode))
            carrierCode = carriersMap.get(carrierCode).getIataCode();
        else
            carrierCode = "";
        if(CommonUtils.isStringNullOrEmpty(flightNumber))
            flightNumber = "";
        String day = etd != null ? String.valueOf(etd.getDayOfMonth()) : "";
        return String.format("%s %s/%s", carrierCode, flightNumber, day);
    }

    public List<String> populateConsigneeData(Map<String, Object> dictionary, PartiesModel shipmentConsignee) {
        List<String> consignee = null;
        List<String> consigneeWoCont = null;
        if(shipmentConsignee != null)
        {
            Map<String, Object> consigneeAddress = shipmentConsignee.getAddressData();
            if(consigneeAddress != null)
            {
                consignee = ReportHelper.getOrgAddressWithPhoneEmail(getValueFromMap(consigneeAddress, COMPANY_NAME), getValueFromMap(consigneeAddress, ADDRESS1),
                        getValueFromMap(consigneeAddress, ADDRESS2),
                        ReportHelper.getCityCountry(getValueFromMap(consigneeAddress, CITY), getValueFromMap(consigneeAddress, COUNTRY)),
                        getValueFromMap(consigneeAddress, "Email"), getValueFromMap(consigneeAddress, CONTACT_PHONE),
                        getValueFromMap(consigneeAddress,ZIP_POST_CODE));

                consigneeWoCont = ReportHelper.getOrgAddressWithoutPhoneEmail(
                        getValueFromMap(consigneeAddress, COMPANY_NAME),
                        getValueFromMap(consigneeAddress, ADDRESS1),
                        getValueFromMap(consigneeAddress, ADDRESS2),
                        getValueFromMap(consigneeAddress, CITY),
                        getValueFromMap(consigneeAddress, STATE),
                        getValueFromMap(consigneeAddress, ZIP_POST_CODE),
                        getValueFromMap(consigneeAddress, COUNTRY)
                );

                dictionary.put(CONSIGNEE_ADD_WITHOUT_CONTACT, consigneeWoCont);
                dictionary.put(ReportConstants.CONSIGNEE_NAME, getValueFromMap(consigneeAddress, COMPANY_NAME));
                dictionary.put(CONSIGNEE_COMPANY_NAME, getValueFromMap(consigneeAddress, COMPANY_NAME));
                dictionary.put(ReportConstants.CONSIGNEE_CONTACT_PERSON,getValueFromMap(consigneeAddress, CONTACT_PERSON_ALIAS));
                String contactPerson = getValueFromMap(consigneeAddress, CONTACT_PERSON_ALIAS);
                dictionary.put(ReportConstants.CONSIGNEE_PIC, contactPerson == null ? "" : contactPerson.toUpperCase());
                dictionary.put(ReportConstants.CONSIGNEE_ADDRESS, ReportHelper.getOrgAddress(shipmentConsignee));

                try {
                    dictionary.put(ReportConstants.CONSIGNEE_PHONE, consigneeAddress.get("ContactPhone"));
                    dictionary.put(ReportConstants.CONSIGNEE_FULL_NAME, shipmentConsignee.getOrgData().get(FULL_NAME1));
                } catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
            }
            String consigneeFullName = getConsigneeFullName(dictionary, shipmentConsignee);

            addConsgineeTextTags(dictionary, shipmentConsignee, consigneeFullName, consignee);
        }
        return consignee;
    }

    private String getConsigneeFullName(Map<String, Object> dictionary, PartiesModel shipmentConsignee) {
        String consigneeFullName = null;
        if(shipmentConsignee.getOrgData() != null) {
            dictionary.put(ReportConstants.CONSIGNEE_LOCAL_NAME, getValueFromMap(shipmentConsignee.getOrgData(), LOCAL_NAME));
            consigneeFullName = getValueFromMap(shipmentConsignee.getOrgData(), ReportConstants.FULL_NAME);
        }
        return consigneeFullName;
    }

    private void addConsgineeTextTags(Map<String, Object> dictionary, PartiesModel shipmentConsignee, String consigneeFullName, List<String> consignee) {
        if (shipmentConsignee.getIsAddressFreeText() != null && shipmentConsignee.getIsAddressFreeText())
        {
            String rawData = shipmentConsignee.getAddressData() != null && shipmentConsignee.getAddressData().containsKey(PartiesConstants.RAW_DATA)? String.valueOf(shipmentConsignee.getAddressData().get(PartiesConstants.RAW_DATA)): null;
            List<String> consigneeRawAddress = ReportHelper.getAddressList(rawData);
            if(consigneeRawAddress != null && !consigneeRawAddress.isEmpty())
            {
                //Display the consignee name, in case of free text needs to display the first line entered in the address.
                dictionary.put(ReportConstants.CONSIGNEE_NAME_FREE_TEXT, consigneeRawAddress.get(0).toUpperCase());
                dictionary.put(CONSIGNEE_FREETEXT, consigneeRawAddress);
                dictionary.put(CONSIGNEE_ADDRESS_FREE_TEXT_IN_CAPS, consigneeRawAddress.stream().map(StringUtility::toUpperCase).toList());
            }
        }
        else
        {
            dictionary.put(ReportConstants.CONSIGNEE_NAME_FREE_TEXT, consigneeFullName == null ? "": consigneeFullName.toUpperCase());
            dictionary.put(CONSIGNEE_FREETEXT, consignee);
        }
    }

    public ShipmentModel getShipment(Long id) {
        ShipmentDetails shipmentDetails = getShipmentDetails(id);
        return getShipment(shipmentDetails);
    }

    public ShipmentDetails getShipmentDetails(Long id) {
        return shipmentDao.findById(id).orElse(null);
    }
    public ShipmentModel getShipmentModel(Long id) {
        return modelMapper.map(getShipmentDetails(id), ShipmentModel.class);
    }
    public ShipmentModel getShipmentByQuery(Long id) {
        var optional = shipmentDao.findShipmentsByIds(Set.of(id)).stream().findFirst();
        return getShipment(optional.orElse(null));
    }

    public ShipmentModel getShipment(ShipmentDetails shipmentDetails) {
        if(shipmentDetails == null) return null;
        ShipmentModel shipmentModel = modelMapper.map(shipmentDetails, ShipmentModel.class);
        shipmentModel.setVoyage(shipmentDetails.getCarrierDetails().getVoyage());
        Map<Long, Containers> containersMap = new HashMap<>();

        try {
            if(shipmentDetails.getContainersList() != null) {
                containersMap = shipmentDetails.getContainersList().stream().collect(Collectors.toMap(BaseEntity::getId, Function.identity()));
                ContainerSummaryResponse containerSummaryResponse = containerService.calculateContainerSummary(jsonHelper.convertValueToList(shipmentDetails.getContainersList(), Containers.class), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType());
                if(containerSummaryResponse != null) {
                    shipmentModel.setSummary(containerSummaryResponse.getSummary());
                }
            }

            if(shipmentDetails.getPackingList() != null) {
                PackSummaryResponse response = packingService.calculatePackSummary(shipmentDetails.getPackingList(), shipmentDetails.getTransportMode(), shipmentDetails.getShipmentType(), new ShipmentMeasurementDetailsDto());
                Map<Long, Containers> finalContainersMap = containersMap;
                shipmentModel.getPackingList().forEach(i -> i.setContainerNumber(
                        Optional.ofNullable(finalContainersMap.get(i.getContainerId()))
                                .map(Containers::getContainerNumber).orElse(null)
                        ));
                if(response != null) {
                    shipmentModel.setPackSummary(response.getTotalPacks());
                }
            }

        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return shipmentModel;
    }

    public Map<Long, ShipmentModel> getShipments(List<Long> shipIds) {
        List<ShipmentDetails> shipmentDetails = shipmentDao.findShipmentsByIds(new HashSet<>(shipIds));
        if(shipmentDetails != null && !shipmentDetails.isEmpty()) {
            Map<Long, ShipmentModel> response = new HashMap<>();
            for (ShipmentDetails shipmentDetails1 : shipmentDetails) {
                ShipmentModel shipmentModel = getShipment(shipmentDetails1);
                response.put(shipmentDetails1.getId(), shipmentModel);
            }
            return response;
        }
        return new HashMap<>();
    }

    public ConsolidationModel getFirstConsolidationFromShipmentId(Long shipmentId)
    {
        List<ConsoleShipmentMapping> consoleShipmentMappings = consoleShipmentMappingDao.findByShipmentIdByQuery(shipmentId);
        if(consoleShipmentMappings != null && !consoleShipmentMappings.isEmpty()) {
            Optional<Long> maxConsolidationId = consoleShipmentMappings.stream().map(ConsoleShipmentMapping::getConsolidationId).max(Comparator.naturalOrder());
            Long consoleId = maxConsolidationId.orElse(null);
            return getConsolidation(consoleId);
        }
        return null;
    }

    public TenantModel getTenant()
    {
        DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().retrieveTenant();
        return modelMapper.map(dependentServiceResponse.getData(), TenantModel.class);
    }

    public ShipmentSettingsDetails getShipmentSettings() {
        ShipmentSettingsDetails tenantSettingsRow = new ShipmentSettingsDetails();
        List<ShipmentSettingsDetails> shipmentSettingsDetailsList = shipmentSettingsDao.getSettingsByTenantIds(Arrays.asList(UserContext.getUser().TenantId));
        if (shipmentSettingsDetailsList != null && !shipmentSettingsDetailsList.isEmpty()) {
            tenantSettingsRow = shipmentSettingsDetailsList.get(0);
        }
        return tenantSettingsRow;
    }

    public ConsolidationModel getConsolidation(Long id) {
        if (id == null)
            return null;
        ConsolidationDetails consolidationDetails = getConsolidationsById(id);
        return getConsolidationModel(consolidationDetails);
    }

    public ConsolidationModel getConsolidationModel(ConsolidationDetails consolidationDetails) {
        return modelMapper.map(consolidationDetails, ConsolidationModel.class);
    }

    public ConsolidationDetails getConsolidationsById(Long id) {
        return consolidationDetailsDao.findConsolidationsById(id);
    }

    public List<BillingResponse> getBillingData(UUID shipmentGuid) {
        if (Boolean.TRUE.equals(billingServiceUrlConfig.getEnableBillingIntegration())) {

            BillRetrieveRequest request = new BillRetrieveRequest();
            request.setModuleGuid(shipmentGuid.toString());
            request.setModuleType(Constants.SHIPMENT);

            BillBaseResponse billFromBilling = billingServiceAdapter.fetchBill(request);

            BillingResponse billingResponse = new BillingResponse();
            billingResponse.setBillId(billFromBilling.getBillId());
            billingResponse.setGuid(!StringUtility.isEmpty(billFromBilling.getGuId()) ? UUID.fromString(billFromBilling.getGuId()) : null);
            billingResponse.setRemarks(billFromBilling.getRemarks());

            return List.of(billingResponse);
        } else {
            ShipmentGuidRequest request = new ShipmentGuidRequest();
            request.setShipmentGuid(shipmentGuid);
            DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().fetchBillingList(request);
            return jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillingResponse.class);
        }
    }

    public List<BillChargesResponse> getBillChargesData(BillingResponse billingResponse) {
        if (Boolean.TRUE.equals(billingServiceUrlConfig.getEnableBillingIntegration())) {

            if(StringUtility.isEmpty(billingResponse.getBillId())) {
                return null;
            }
            List<String> billIds = List.of(billingResponse.getBillId());

            BillChargesFilterRequest request = new BillChargesFilterRequest();
            request.setBillId(billIds);

            List<BillChargesBaseResponse> billChargesFromBilling = billingServiceAdapter.fetchBillCharges(request);
            return this.convertBillingBillChargeToRunnerBillCharge(billChargesFromBilling);
        } else {
            List<Object> criteria = new ArrayList<>();
            criteria.add(Arrays.asList(List.of("BillId"), "=", billingResponse.getGuid()));
            criteria.add("and");
            criteria.add(Arrays.asList(List.of("IsActive"), "=", 1));
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
            DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().fetchBillChargesList(commonV1ListRequest);
            return jsonHelper.convertValueToList(dependentServiceResponse.getData(), BillChargesResponse.class);
        }
    }

    private List<BillChargesResponse> convertBillingBillChargeToRunnerBillCharge(List<BillChargesBaseResponse> billChargesBaseResponses) {
        List<BillChargesResponse> v1BillCharges = new ArrayList<>();

        for (BillChargesBaseResponse billingBillCharge : billChargesBaseResponses) {
            if (billingBillCharge == null) {
                continue; // Skip null objects
            }

            BillChargesResponse v1BillCharge = new BillChargesResponse();
            // Use Optionals for null checks
            Optional<BillChargeRevenueDetailsResponse> revenueDetailsOpt = Optional.ofNullable(billingBillCharge.getBillChargeRevenueDetails());
            Optional<BillChargeCostDetailsResponse> costDetailsOpt = Optional.ofNullable(billingBillCharge.getBillChargeCostDetails());
            Optional<ChargeTypeBaseResponse> chargeTypeDetailsOpt = Optional.ofNullable(billingBillCharge.getChargeTypeDetails());

            v1BillCharge.setBillingChargeTypeId(billingBillCharge.getChargeTypeId());
            v1BillCharge.setBillingChargeTypeGuid(
                    chargeTypeDetailsOpt.map(ChargeTypeBaseResponse::getGuId)
                            .map(UUID::toString)
                            .orElse(null)
            );
            v1BillCharge.setOverseasSellAmount(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getOverseasSellAmount).orElse(null)
            );
            v1BillCharge.setOverseasSellCurrency(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getOverseasSellCurrency).orElse(null)
            );
            v1BillCharge.setLocalSellAmount(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getLocalSellAmount).orElse(null)
            );
            v1BillCharge.setLocalSellCurrency(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getLocalSellCurrency).orElse(null)
            );
            v1BillCharge.setOverseasTax(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getOverseasTax).orElse(null)
            );
            v1BillCharge.setSellExchange(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getCurrencyExchangeRateDetails)
                            .orElse(Collections.emptyList())
                            .stream()
                            .filter(currencyExRate -> ExchangeRateType.CUSTOMER.equals(currencyExRate.getType()))
                            .findFirst()
                            .map(CurrencyExchangeRateDetailsResponse::getExchangeRate)
                            .orElse(null)
            );
            v1BillCharge.setTaxType1(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getTaxDetails)
                            .orElse(Collections.emptyList())
                            .stream()
                            .filter(tax -> TaxType.IGST.equals(tax.getTaxType()) || TaxType.VAT.equals(tax.getTaxType()))
                            .findFirst()
                            .map(TaxDetailsResponse::getAmount)
                            .orElse(null)
            );
            v1BillCharge.setTaxType2(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getTaxDetails)
                            .orElse(Collections.emptyList())
                            .stream()
                            .filter(tax -> TaxType.SGST.equals(tax.getTaxType()))
                            .findFirst()
                            .map(TaxDetailsResponse::getAmount)
                            .orElse(null)
            );
            v1BillCharge.setTaxType3(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getTaxDetails)
                            .orElse(Collections.emptyList())
                            .stream()
                            .filter(tax -> TaxType.CGST.equals(tax.getTaxType()))
                            .findFirst()
                            .map(TaxDetailsResponse::getAmount)
                            .orElse(null)
            );
            v1BillCharge.setTaxType4(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getTaxDetails)
                            .orElse(Collections.emptyList())
                            .stream()
                            .filter(tax -> TaxType.UGST.equals(tax.getTaxType()))
                            .findFirst()
                            .map(TaxDetailsResponse::getAmount)
                            .orElse(null)
            );
            v1BillCharge.setLocalTax(
                    revenueDetailsOpt.map(BillChargeRevenueDetailsResponse::getTaxAmount).orElse(null)
            );
            v1BillCharge.setMeasurementBasis(null); // LATER: Check for cost/revenue MeasurementBasis

            v1BillCharge.setPaymentType(billingBillCharge.getPaymentTypeCode());
            v1BillCharge.setChargeTypeCode(
                    chargeTypeDetailsOpt.map(ChargeTypeBaseResponse::getChargeCode).orElse(null)
            );
            v1BillCharge.setChargeTypeDescription(
                    chargeTypeDetailsOpt.map(ChargeTypeBaseResponse::getChargeCodeDescription).orElse(null)
            );
            v1BillCharge.setLocalCostCurrency(
                    costDetailsOpt.map(BillChargeCostDetailsResponse::getLocalCostCurrency).orElse(null)
            );

            v1BillCharges.add(v1BillCharge);
        }

        return v1BillCharges;
    }

    public List<ArObjectResponse> getArObjectData(UUID billGuid) {
        List<Object> criteria = new ArrayList<>();
        criteria.add(Arrays.asList(List.of("BillId"), "=", billGuid));
        criteria.add("and");
        criteria.add(Arrays.asList(List.of("IsActive"), "=", 1));
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
        DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().fetchArObjectList(commonV1ListRequest);
        return jsonHelper.convertValueToList(dependentServiceResponse.getData(), ArObjectResponse.class);
    }

    public ChargeTypesResponse getChargeTypesData(BillChargesResponse billChargesResponse) {
        if (Boolean.TRUE.equals(billingServiceUrlConfig.getEnableBillingIntegration())) {
            ChargeTypesResponse v1ChargeType = new ChargeTypesResponse();

            ChargeTypeFilterRequest request = new ChargeTypeFilterRequest();
            request.setGuidList(List.of(billChargesResponse.getBillingChargeTypeGuid()));

            List<ChargeTypeBaseResponse> chargeTypesFromBilling = billingServiceAdapter.fetchChargeTypes(request);
            ChargeTypeBaseResponse chargeTypeFromBilling = chargeTypesFromBilling.stream().findFirst().orElse(null);
            v1ChargeType.setServices(
                    Optional.ofNullable(chargeTypeFromBilling)
                            .map(ChargeTypeBaseResponse::getChargeGroup).orElse(null)
            );
            return v1ChargeType;
        } else {
            List<Object> criteria = Arrays.asList(
                    List.of("Id"),
                    "=",
                    billChargesResponse.getChargeTypeId()
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
            DependentServiceResponse dependentServiceResponse = masterDataFactory.getMasterDataService().fetchChargeType(commonV1ListRequest);
            List<ChargeTypesResponse> chargeTypesResponses = jsonHelper.convertValueToList(dependentServiceResponse.getData(), ChargeTypesResponse.class);
            if (CollectionUtils.isNotEmpty(chargeTypesResponses)) {
                return chargeTypesResponses.get(0);
            }
            return null;
        }
    }

    public VesselsResponse getVesselsData(String guid) {
        if (isStringNullOrEmpty(guid)) {
            return null;
        }
        List<Object> vesselCriteria = Arrays.asList(
                List.of(Constants.VESSEL_GUID_V1),
                "=",
                guid
        );
        CommonV1ListRequest vesselRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(vesselCriteria).build();
        V1DataResponse vesselResponse = v1Service.fetchVesselData(vesselRequest);
        List<VesselsResponse> vesselsResponse = jsonHelper.convertValueToList(vesselResponse.entities, VesselsResponse.class);
        if(vesselsResponse != null && !vesselsResponse.isEmpty())
            return vesselsResponse.get(0);
        return null;
    }

    public Hbl getHbl(Long id) {
        List<Hbl> hbls = hblDao.findByShipmentId(id);
        if(hbls != null && !hbls.isEmpty())
            return hbls.get(0);
        return null;
    }

    public void populateConsolidationFields(ConsolidationModel consolidation, Map<String, Object> dictionary) {
        if (consolidation == null) {
            return;
        }
        var shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();

        PartiesModel sendingAgent = consolidation.getSendingAgent();
        PartiesModel receivingAgent = consolidation.getReceivingAgent();
        PartiesModel creditor = consolidation.getCreditor();
        ArrivalDepartureDetailsModel arrivalDetails = consolidation.getArrivalDetails();
        ArrivalDepartureDetailsModel departureDetails = consolidation.getDepartureDetails();
        List<MasterListRequest> masterListRequest = createMasterListsRequestFromConsole(consolidation);
        Map<Integer, Map<String, MasterData>> masterListsMap = fetchInBulkMasterList(MasterListRequestV2.builder().MasterListRequests(masterListRequest.stream().filter(Objects::nonNull).toList()).build());
        UnlocationsResponse lastForeignPort = getLastForeignPort(arrivalDetails);
        dictionary.put(ReportConstants.CARGO_CLOSING_TIME, consolidation.getCargoClosingTime());
        dictionary.put(ReportConstants.DOCS_CLOSING_TIME, consolidation.getDocsClosingTime());
        dictionary.put(ReportConstants.MASTER_BILL, consolidation.getBol());
        List<String> exportAgentAddress = new ArrayList<>();
        List<String> importAgentAddress = new ArrayList<>();
        exportAgentAddress = processSendingAgent(dictionary, sendingAgent, exportAgentAddress);
        importAgentAddress = processReceivingAgent(dictionary, receivingAgent, importAgentAddress);

        List<String> exportAgentFreeTextAddress = getExportAgentFreeTextAddress(consolidation, exportAgentAddress);

        List<String> importAgentFreeTextAddress = getImportAgentFreeTextAddress(consolidation, importAgentAddress);

        addExportImportAgentTags(consolidation, dictionary, exportAgentAddress, importAgentAddress, exportAgentFreeTextAddress, importAgentFreeTextAddress);

        dictionary.put(ReportConstants.SHIPMENT_TYPE, consolidation.getShipmentType());
        dictionary.put(ReportConstants.CUSTOM_SHIPMENT_TYPE, !StringUtility.isEmpty(consolidation.getShipmentType())
                ? consolidation.getShipmentType().toUpperCase().charAt(0) : null);

        dictionary.put(ReportConstants.CONSOL_VESSEL_NAME, consolidation.getCarrierDetails().getVessel());
        dictionary.put(ReportConstants.CONSOL_VOYAGE, consolidation.getCarrierDetails().getVoyage());
        addCreditorTags(dictionary, creditor);

        dictionary.put(ReportConstants.CONSOL_DATE_OF_DEPARTURE, dictionary.get(ReportConstants.CONSOL_ATD) != null ? dictionary.get(ReportConstants.CONSOL_ATD) : dictionary.get(ReportConstants.CONSOL_ETD));

        addArrivalDetailsTag(dictionary, arrivalDetails, lastForeignPort);
        dictionary.put(ReportConstants.MSN_NUMBER, consolidation.getMsnNumber());
        dictionary.put(ReportConstants.MRN_NUMBER, consolidation.getMrnNumber());
        dictionary.put(ReportConstants.MASTER_BILL_ISSUE_DATE, consolidation.getMasterBillIssueDate());
        dictionary.put(ReportConstants.CONSOL_REFERENCE_NUMBER, consolidation.getReferenceNumber());
        dictionary.put(ReportConstants.CONSOL_ADDITIONAL_TERMS, consolidation.getAdditionalTerms());
        processConsoleCarrierDetails(consolidation, dictionary);

        addNotifyAddressTag(consolidation.getConsolidationAddresses(), dictionary, ReportConstants.CONSOL_NOTIFY_ADDRESS);
        dictionary.put(CONSOLE_DELIVERY_MODE_IN_CAPS, StringUtility.toUpperCase(consolidation.getDeliveryMode()));
        dictionary.put(CONSOLE_MARKS_N_NUMBERS_IN_CAPS, StringUtility.toUpperCase(consolidation.getMrnNumber()));
        MasterData masterdata = MasterData.builder().build();
        if (masterListsMap.containsKey(MasterDataType.PAYMENT.getId()) && masterListsMap.get(MasterDataType.PAYMENT.getId()).containsKey(consolidation.getPayment()))
            masterdata = masterListsMap.get(MasterDataType.PAYMENT.getId()).get(consolidation.getPayment());
        dictionary.put(CONSOLE_PAYMENT_TERMS_IN_CAPS, StringUtility.toUpperCase(masterdata != null ? masterdata.getItemDescription() : consolidation.getPayment()));
        dictionary.put(CONSOLE_PAYMENT_TERMS, StringUtility.toUpperCase(consolidation.getPayment()));

        addConsolidationAllocationTags(consolidation, dictionary, masterListsMap);
        dictionary.put(SI_CUT_OFF_TIME, consolidation.getShipInstructionCutoff());
        if(departureDetails != null) {
            if(departureDetails.getCTOId() != null){
                dictionary.put(CTO_FULL_NAME, getValueFromMap(departureDetails.getCTOId().getOrgData(), FULL_NAME));
                dictionary.put(TERMINAL, dictionary.get(CTO_FULL_NAME));
            }
            if(departureDetails.getContainerYardId() != null) {
                List<String> cyNameAddress = new ArrayList<>();
                if(!Boolean.TRUE.equals(shipmentSettingsDetails.getDisableBlPartiesName()))
                    cyNameAddress.add(getValueFromMap(departureDetails.getContainerYardId().getOrgData(), FULL_NAME));
                cyNameAddress.addAll(getOrgAddress(departureDetails.getContainerYardId()));
                dictionary.put(CY_NAME_ADDRESS, String.join("\r\n", cyNameAddress));
            }
        }
        populateUserFields(UserContext.getUser(), dictionary);
        dictionary.put(IS_DG, false);
        if(Boolean.TRUE.equals(consolidation.getHazardous())) {
            dictionary.put(IS_DG, true);
            dictionary.put(DG_EMERGENCY_CONTACT, getConcatenatedContact(consolidation.getEmergencyContactNumberCode(), consolidation.getEmergencyContactNumber()));
        }
        dictionary.put(MAWB_CAPS, StringUtility.convertToString(consolidation.getMawb()));
    }

    private void addConsolidationAllocationTags(ConsolidationModel consolidation, Map<String, Object> dictionary, Map<Integer, Map<String, MasterData>> masterListsMap) {
        if (consolidation.getAllocations() != null) {
            dictionary.put(CONSOLIDATED_WEIGHT, addCommas(consolidation.getAllocations().getWeight()));
            dictionary.put(CONSOLE_WEIGHT_CODE, StringUtility.toUpperCase(consolidation.getAllocations().getWeightUnit()));
            dictionary.put(CONSOLIDATED_VOLUME, addCommas(consolidation.getAllocations().getVolume()));
            dictionary.put(CONSOLE_VOLUME_CODE, StringUtility.toUpperCase(consolidation.getAllocations().getVolumeUnit()));
            if (masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT.getId()) && masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).containsKey(consolidation.getAllocations().getWeightUnit()))
                dictionary.put(CONSOLE_WEIGHT_DESCRIPTION, StringUtility.toUpperCase(masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).get(consolidation.getAllocations().getWeightUnit()).getItemDescription()));
            if (masterListsMap.containsKey(MasterDataType.VOLUME_UNIT.getId()) && masterListsMap.get(MasterDataType.VOLUME_UNIT.getId()).containsKey(consolidation.getAllocations().getVolumeUnit()))
                dictionary.put(CONSOLE_VOLUME_DESCRIPTION, StringUtility.toUpperCase(masterListsMap.get(MasterDataType.VOLUME_UNIT.getId()).get(consolidation.getAllocations().getVolumeUnit()).getItemDescription()));
        }
    }

    private void addArrivalDetailsTag(Map<String, Object> dictionary, ArrivalDepartureDetailsModel arrivalDetails, UnlocationsResponse lastForeignPort) {
        if(arrivalDetails != null)
        {
            dictionary.put(ReportConstants.LAST_FOREIGN_PORT, lastForeignPort != null ? lastForeignPort.getName() : null);
            dictionary.put(ReportConstants.LAST_FORGEIN_PORT_COUNTRY, lastForeignPort != null ? lastForeignPort.getCountry() : null);
            dictionary.put(ReportConstants.DEST_PORT_NAME, lastForeignPort != null ? lastForeignPort.getName() : null);
            dictionary.put(ReportConstants.DEST_PORT_COUNTRY, lastForeignPort != null ? lastForeignPort.getCountry() : null);
        }
    }

    private void processConsoleCarrierDetails(ConsolidationModel consolidation, Map<String, Object> dictionary) {
        if(consolidation.getCarrierDetails() != null) {
            dictionary.put(ReportConstants.CONSOL_ETA, consolidation.getCarrierDetails().getEta());
            dictionary.put(ReportConstants.CONSOL_ETD, consolidation.getCarrierDetails().getEtd());
            dictionary.put(ReportConstants.CONSOL_ATA, consolidation.getCarrierDetails().getAta());
            dictionary.put(ReportConstants.CONSOL_ATD, consolidation.getCarrierDetails().getAtd());
            dictionary.put(ReportConstants.CONSOL_FLIGHT_NUMBER, consolidation.getCarrierDetails().getFlightNumber());
            addCarrierDetailsShippingLineTags(consolidation, dictionary);
            List<String> unlocoRequests = createUnLocoRequestFromConsolidation(consolidation);
            Map<String, UnlocationsResponse> unlocationsMap = masterDataUtils.getLocationData(new HashSet<>(unlocoRequests));
            UnlocationsResponse pol = consolidation.getCarrierDetails() != null ? unlocationsMap.get(consolidation.getCarrierDetails().getOriginPort()) : null;
            UnlocationsResponse pod = consolidation.getCarrierDetails() != null ? unlocationsMap.get(consolidation.getCarrierDetails().getDestinationPort()) : null;
            UnlocationsResponse origin = consolidation.getCarrierDetails() != null ? unlocationsMap.get(consolidation.getCarrierDetails().getOrigin()) : null;
            UnlocationsResponse destination = consolidation.getCarrierDetails() != null ? unlocationsMap.get(consolidation.getCarrierDetails().getDestination()) : null;
            addCarrierDetailsTag(dictionary, pol, pod, origin, destination);
        }
    }

    private void addNotifyAddressTag(List<PartiesModel> partiesModelList, Map<String, Object> dictionary, String notifyAddress) {
        PartiesModel notifyParty = null;
        for (PartiesModel partiesModel : partiesModelList) {
            if (Objects.equals(partiesModel.getType(), "Notify Party 1")) {
                notifyParty = partiesModel;
            }
        }
        if (notifyParty != null) {
            Map<String, Object> addressData = notifyParty.getAddressData();
            List<String> consolNotifyPartyAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                    StringUtility.convertToString(addressData.get(ADDRESS2)),
                    ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                    null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                    StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
            );
            dictionary.put(notifyAddress, consolNotifyPartyAddress);
        }
    }

    private void addCarrierDetailsTag(Map<String, Object> dictionary, UnlocationsResponse pol, UnlocationsResponse pod, UnlocationsResponse origin, UnlocationsResponse destination) {
        addPolTags(dictionary, pol);

        addPodTags(dictionary, pod);

        if(origin!=null){
            if(origin.getPortName() != null) {
                dictionary.put(ReportConstants.ORIGIN, origin.getPortName().toUpperCase());
            }
            if(origin.getAirPortName() != null) {
                dictionary.put(ReportConstants.ORIGIN_AIR, origin.getAirPortName().toUpperCase());
            }
        }

        if(destination!=null){
            if(destination.getPortName() != null) {
                dictionary.put(ReportConstants.DESTINATION, destination.getPortName().toUpperCase());
            }
            if(destination.getAirPortName() != null) {
                dictionary.put(ReportConstants.DESTINATION_AIR, destination.getAirPortName().toUpperCase());
            }
        }
    }

    private void addPodTags(Map<String, Object> dictionary, UnlocationsResponse pod) {
        if(pod !=null){
            if (pod.getPortName() != null) {
                dictionary.put(ReportConstants.DESTINATION_PORT_NAME_INCAPS, pod.getPortName().toUpperCase());
            }
            if (!CommonUtils.isStringNullOrEmpty(pod.getIataCode())) {
                dictionary.put(ReportConstants.CONSOL_DESTINATION_AIRPORT_CODE, pod.getIataCode());
                dictionary.put(CONSOL_DESTINATION_AIRPORT_CODE_CAPS, pod.getIataCode().toUpperCase());
            }
            if (pod.getAirPortName() != null) {
                dictionary.put(ReportConstants.DESTINATION_PORT_NAME_INCAPS_AIR, pod.getAirPortName().toUpperCase());
            }
        }
    }

    private void addPolTags(Map<String, Object> dictionary, UnlocationsResponse pol) {
        if(pol !=null){
            if (pol.getPortName() != null) {
                dictionary.put(AIRLINE_NAME, pol.getPortName().toUpperCase());
                dictionary.put(ReportConstants.ORIGIN_PORT_NAME_INCAPS, pol.getPortName().toUpperCase());
            }
            if (!CommonUtils.isStringNullOrEmpty(pol.getIataCode())) {
                dictionary.put(ReportConstants.CONSOL_ORIGIN_AIRPORT_CODE, pol.getIataCode());
                dictionary.put(CONSOL_ORIGIN_AIRPORT_CODE_CAPS, pol.getIataCode().toUpperCase());
            }
            if (pol.getAirPortName() != null) {
                dictionary.put(ReportConstants.ORIGIN_PORT_NAME_INCAPS_AIR, pol.getAirPortName().toUpperCase());
            }
        }
    }

    private void addCarrierDetailsShippingLineTags(ConsolidationModel consolidation, Map<String, Object> dictionary) {
        if(StringUtility.isNotEmpty(consolidation.getCarrierDetails().getShippingLine())) {
            CarrierMasterData carriersData = getCarrier(consolidation.getCarrierDetails().getShippingLine());
            if (carriersData != null) {
                String iataCode = carriersData.getIataCode();
                dictionary.put(ReportConstants.CONSOL_FLIGHT_NUMBER_WITH_IATACODE,StringUtility.isEmpty(iataCode) ? consolidation.getCarrierDetails().getFlightNumber() : iataCode + " " + consolidation.getCarrierDetails().getFlightNumber());
                dictionary.put(ReportConstants.IATA_CODE, StringUtility.isEmpty(iataCode) ? consolidation.getCarrierDetails().getFlightNumber() : iataCode + getConsolidationFlightNumber(consolidation));
                dictionary.put(ReportConstants.CARRIER_IATACODE, iataCode);
                dictionary.put(CARRIER_NAME, StringUtility.toUpperCase(carriersData.getItemDescription()));
                dictionary.put(CARRIER_CONTACT_PERSON, StringUtility.toUpperCase(carriersData.getCarrierContactPerson()));
                addDefaultOrgTags(dictionary, carriersData);
            }
        }
    }

    private String getConsolidationFlightNumber(ConsolidationModel consolidation) {
        return StringUtility.isEmpty(consolidation.getCarrierDetails().getFlightNumber()) ? "" : (" " + consolidation.getCarrierDetails().getFlightNumber());
    }

    private void addDefaultOrgTags(Map<String, Object> dictionary, CarrierMasterData carriersData) {
        if (carriersData.getDefaultOrgId() != 0) {
            List<EntityTransferOrganizations> orgs = masterDataUtils.fetchOrganizations("Id", carriersData.getDefaultOrgId());
            if (orgs != null && !orgs.isEmpty()) {
                dictionary.put(ReportConstants.CARRIER_ORG_NAME, StringUtility.toUpperCase(orgs.get(0).getFullName()));
                dictionary.put(ReportConstants.CARRIER_ORG_PHONE, orgs.get(0).getPhone());
                dictionary.put(ReportConstants.CARRIER_ORG_FAX, orgs.get(0).getFax());
            }
        }
    }

    private void addCreditorTags(Map<String, Object> dictionary, PartiesModel creditor) {
        List<String> creditorAgentAddress;
        if(creditor != null)
        {
            Map<String, Object> orgData = creditor.getOrgData();
            Map<String, Object> addressData = creditor.getAddressData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.CREDITOR, Arrays.asList(orgData.get(FULL_NAME), creditor.getAddressData() != null ? creditor.getAddressData().get(ADDRESS1) : null));
                dictionary.put(ReportConstants.CREDITOR_LOCAL_NAME, orgData.get(LOCAL_NAME));
                dictionary.put(ReportConstants.CREDITOR_FULL_NAME, orgData.get(FULL_NAME));
            }
            creditorAgentAddress = ReportHelper.getOrgAddress(creditor);
            if (addressData != null)
                dictionary.put(CREDITOR_AGENT_NAME, StringUtility.toUpperCase(StringUtility.convertToString(addressData.get(COMPANY_NAME))));

            List<String> creditorAgentFreeTextAddress;
            if (!Objects.isNull(addressData) && addressData.containsKey(PartiesConstants.RAW_DATA)) {
                creditorAgentFreeTextAddress = ReportHelper.getAddressList(StringUtility.convertToString(addressData.get(PartiesConstants.RAW_DATA)));
            }
            else {
                creditorAgentFreeTextAddress = creditorAgentAddress;
            }
            dictionary.put(ReportConstants.CREDITOR_AGENT_FREETEXT, creditorAgentFreeTextAddress.stream().map(StringUtility::toUpperCase).toList());
        }
    }

    private void addExportImportAgentTags(ConsolidationModel consolidation, Map<String, Object> dictionary, List<String> exportAgentAddress, List<String> importAgentAddress, List<String> exportAgentFreeTextAddress, List<String> importAgentFreeTextAddress) {
        if (Objects.equals(consolidation.getShipmentType(), "EXP"))
        {
            dictionary.put(ReportConstants.EXPORT_AGENT, exportAgentAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT, importAgentAddress);
            dictionary.put(ReportConstants.EXPORT_AGENT_FREETEXT, exportAgentFreeTextAddress.stream().map(StringUtility::toUpperCase).toList());
            dictionary.put(ReportConstants.IMPORT_AGENT_FREETEXT, importAgentFreeTextAddress.stream().map(StringUtility::toUpperCase).toList());
        }
        else
        {
            dictionary.put(ReportConstants.EXPORT_AGENT, importAgentAddress);
            dictionary.put(ReportConstants.IMPORT_AGENT, exportAgentAddress);
            dictionary.put(ReportConstants.EXPORT_AGENT_FREETEXT, importAgentFreeTextAddress.stream().map(StringUtility::toUpperCase).toList());
            dictionary.put(ReportConstants.IMPORT_AGENT_FREETEXT, exportAgentFreeTextAddress.stream().map(StringUtility::toUpperCase).toList());
        }
    }

    private List<String> getImportAgentFreeTextAddress(ConsolidationModel consolidation, List<String> importAgentAddress) {
        List<String> importAgentFreeTextAddress = new ArrayList<>();
        if (consolidation.getReceivingAgent() != null && consolidation.getIsReceivingAgentFreeTextAddress() != null && Boolean.TRUE.equals(consolidation.getIsReceivingAgentFreeTextAddress()))
        {
            Map<String, Object> importAgentAddressData = consolidation.getReceivingAgent().getAddressData();
            if (importAgentAddressData != null && importAgentAddressData.containsKey(PartiesConstants.RAW_DATA))
                importAgentFreeTextAddress = ReportHelper.getAddressList(StringUtility.convertToString(importAgentAddressData.get(PartiesConstants.RAW_DATA)));
        }
        else  {
            importAgentFreeTextAddress = importAgentAddress;
        }
        return importAgentFreeTextAddress;
    }

    private List<String> getExportAgentFreeTextAddress(ConsolidationModel consolidation, List<String> exportAgentAddress) {
        List<String> exportAgentFreeTextAddress = new ArrayList<>();
        if (consolidation.getSendingAgent() != null && consolidation.getIsSendingAgentFreeTextAddress() != null && Boolean.TRUE.equals(consolidation.getIsSendingAgentFreeTextAddress())) {
            Map<String, Object> sendingAgentAddressData = consolidation.getSendingAgent().getAddressData();
            if (sendingAgentAddressData != null && sendingAgentAddressData.containsKey(PartiesConstants.RAW_DATA))
                exportAgentFreeTextAddress = ReportHelper.getAddressList(StringUtility.convertToString(sendingAgentAddressData.get(PartiesConstants.RAW_DATA)));
        }
        else {
            exportAgentFreeTextAddress = exportAgentAddress;
        }
        return exportAgentFreeTextAddress;
    }

    private List<String> processReceivingAgent(Map<String, Object> dictionary, PartiesModel receivingAgent, List<String> importAgentAddress) {
        if(receivingAgent != null)
        {
            Map<String, Object> addressData = receivingAgent.getAddressData();
            if(addressData != null)
            {
                importAgentAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                        StringUtility.convertToString(addressData.get(ADDRESS2)),
                        ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                        null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                        StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
                );
                dictionary.put(ReportConstants.RECEIVING_AGENT_NAME, StringUtility.toUpperCase(StringUtility.convertToString(addressData.get(COMPANY_NAME))));
            }
            Map<String, Object> orgData = receivingAgent.getOrgData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.RECEIVING_AGENT_LOCAL_NAME, orgData.get(LOCAL_NAME));
                dictionary.put(ReportConstants.RECEIVING_AGENT_FULL_NAME, orgData.get(FULL_NAME));
            }
        }
        return importAgentAddress;
    }

    private List<String> processSendingAgent(Map<String, Object> dictionary, PartiesModel sendingAgent, List<String> exportAgentAddress) {
        if(sendingAgent != null)
        {
            Map<String, Object> addressData = sendingAgent.getAddressData();
            if(addressData != null)
            {
                exportAgentAddress = ReportHelper.getOrgAddressWithPhoneEmail(StringUtility.convertToString(addressData.get(COMPANY_NAME)), StringUtility.convertToString(addressData.get(ADDRESS1)),
                        StringUtility.convertToString(addressData.get(ADDRESS2)),
                        ReportHelper.getCityCountry(StringUtility.convertToString(addressData.get(CITY)), StringUtility.convertToString(addressData.get(COUNTRY))),
                        null, StringUtility.convertToString(addressData.get(CONTACT_PHONE)),
                        StringUtility.convertToString(addressData.get(ZIP_POST_CODE))
                );
                dictionary.put(ReportConstants.SENDING_AGENT_NAME, StringUtility.toUpperCase(StringUtility.convertToString(addressData.get(COMPANY_NAME))));
            }
            Map<String, Object> orgData = sendingAgent.getOrgData();
            if(orgData != null)
            {
                dictionary.put(ReportConstants.SENDING_AGENT_LOCAL_NAME, orgData.get(LOCAL_NAME));
                dictionary.put(ReportConstants.SENDING_AGENT_FULL_NAME, orgData.get(FULL_NAME));
            }
        }
        return exportAgentAddress;
    }

    private UnlocationsResponse getLastForeignPort(ArrivalDepartureDetailsModel arrivalDetails) {
        UnlocationsResponse lastForeignPort = null;
        if (arrivalDetails != null && !isStringNullOrEmpty(arrivalDetails.getLastForeignPort())) {
            List<Object> criteria = Arrays.asList(
                    List.of(EntityTransferConstants.LOCATION_SERVICE_GUID),
                    "=",
                    arrivalDetails.getLastForeignPort()
            );
            CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
            Object unlocations = masterDataFactory.getMasterDataService().fetchAllUnlocationData(commonV1ListRequest).getData();
            List<UnlocationsResponse> unlocationsResponse = jsonHelper.convertValueToList(unlocations, UnlocationsResponse.class);
            if(!unlocationsResponse.isEmpty())
                lastForeignPort = unlocationsResponse.get(0);
        }
        return lastForeignPort;
    }

    public void populateBlFields(Hbl hbl, Map<String, Object> dictionary)
    {
        if (hbl == null) return;
        var shipmentSettingsDetails = commonUtils.getShipmentSettingFromContext();
        List<String> notify = getNotifyParty(hbl, shipmentSettingsDetails);
        List<MasterListRequest> masterListRequest = createMasterListsRequestFromHbl(hbl);
        Map<Integer, Map<String, MasterData>> masterListsMap = fetchInBulkMasterList(MasterListRequestV2.builder().MasterListRequests(masterListRequest.stream().filter(Objects::nonNull).toList()).build());
        dictionary.put(ReportConstants.BL_NOTIFY_PARTY, notify);
        dictionary.put(ReportConstants.BL_NOTIFY_PARTY_CAPS, notify.stream().map(String::toUpperCase).toList());
        HblDataDto hblDataDto = hbl.getHblData();
        processConsignorConsigneeTags(dictionary, shipmentSettingsDetails, hblDataDto);

        if(!notify.isEmpty()) {
            dictionary.put(ReportConstants.NOTIFY_PARTY_NAME_FREETEXT_INCAPS, notify.stream().map(StringUtility::toUpperCase).toList());
        }

        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();
        dictionary.put(ReportConstants.PRINT_DATE, convertToDPWDateFormat(LocalDateTime.now(), tsDateTimeFormat, v1TenantSettingsResponse));

        if(StringUtility.isNotEmpty(hblDataDto.getCargoDescription())) {
            dictionary.put(ReportConstants.DO_MESSAGE, hblDataDto.getCargoDescription());
        }
        if (!StringUtility.isEmpty(hblDataDto.getHouseBill()))
            dictionary.put(ReportConstants.HOUSE_BILL, hblDataDto.getHouseBill());
        dictionary.put(ReportConstants.BL_VESSEL_NAME, hblDataDto.getVesselName());
        dictionary.put(ReportConstants.BL_VOYAGE, hblDataDto.getVoyage());
        dictionary.put(ReportConstants.PORT_OF_LOADING, hblDataDto.getPortOfLoad());
        dictionary.put(ReportConstants.PORT_OF_DISCHARGE, hblDataDto.getPortOfDischarge());
        dictionary.put(ReportConstants.MARKS_N_NUMS, hblDataDto.getMarksAndNumbers());
        dictionary.put(ReportConstants.MARKS_N_NUMS_CAPS, hblDataDto.getMarksAndNumbers() != null ? hblDataDto.getMarksAndNumbers().toUpperCase() : null);
        dictionary.put(ReportConstants.PACKS, getDPWWeightVolumeFormat(hblDataDto.getPackageCount() == null ? null : BigDecimal.valueOf(hblDataDto.getPackageCount()), 0, v1TenantSettingsResponse));
        dictionary.put(ReportConstants.PACKS_UNIT, Constants.MPK.equals(hblDataDto.getPackageType()) ? Constants.PACKAGES : hblDataDto.getPackageType());
        if(StringUtility.isNotEmpty(hblDataDto.getCargoDescription())) {
            dictionary.put(ReportConstants.DESCRIPTION, hblDataDto.getCargoDescription());
            dictionary.put(ReportConstants.DESCRIPTION_CAPS, hblDataDto.getCargoDescription() != null ? hblDataDto.getCargoDescription().toUpperCase() : null);
        }
        dictionary.put(ReportConstants.PLACE_OF_DELIVERY, hblDataDto.getPlaceOfDelivery());
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT, convertToWeightNumberFormat(hblDataDto.getCargoNetWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CARGO_NET_WEIGHT_UNIT, hblDataDto.getCargoNetWeightUnit());
        dictionary.put(ReportConstants.FINAL_DESTINATION, hblDataDto.getFinalDestination());
        dictionary.put(ReportConstants.CARGO_GROSS_VOLUME, convertToVolumeNumberFormat(hblDataDto.getCargoGrossVolume(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CARGO_GROSS_VOLUME_UNIT, hblDataDto.getCargoGrossVolumeUnit());
        if (masterListsMap.containsKey(MasterDataType.VOLUME_UNIT.getId()) && masterListsMap.get(MasterDataType.VOLUME_UNIT.getId()).containsKey(hblDataDto.getCargoGrossVolumeUnit()))
            dictionary.put(CARGO_GROSS_VOLUME_UNIT_DESCRIPTION, StringUtility.toUpperCase(StringUtility.convertToString(masterListsMap.get(MasterDataType.VOLUME_UNIT.getId()).get(hblDataDto.getCargoGrossVolumeUnit()))));
        dictionary.put(ReportConstants.CARGO_GROSS_WEIGHT, convertToWeightNumberFormat(hblDataDto.getCargoGrossWeight(), v1TenantSettingsResponse));
        dictionary.put(ReportConstants.CARGO_GROSS_WEIGHT_UNIT, hblDataDto.getCargoGrossWeightUnit());
        if (masterListsMap.containsKey(MasterDataType.WEIGHT_UNIT.getId()) && masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).containsKey(hblDataDto.getCargoGrossVolumeUnit()))
            dictionary.put(CARGO_GROSS_WEIGHT_UNIT_DESCRIPTION, StringUtility.toUpperCase(StringUtility.convertToString(masterListsMap.get(MasterDataType.WEIGHT_UNIT.getId()).get(hblDataDto.getCargoGrossWeightUnit()))));
        dictionary.put(ReportConstants.CARGO_GROSS_PACKAGE_COUNT, hblDataDto.getPackageCount());
        dictionary.put(ReportConstants.CARGO_GROSS_PACKAGE_TYPE, hblDataDto.getPackageType());
        dictionary.put(ReportConstants.CARGO_GROSS_QUANTITY, hblDataDto.getQuantity());
        dictionary.put(ReportConstants.CARGO_GROSS_QUANTITY_CODE, hblDataDto.getQuantityCode());
        dictionary.put(ReportConstants.PLACE_OF_RECEIPT, hblDataDto.getPlaceOfReceipt());
        if(StringUtility.isNotEmpty(hblDataDto.getPlaceOfReceipt())) {
            dictionary.put(ReportConstants.PLACE_OF_RECIEPT_IN_CAPS, hblDataDto.getPlaceOfReceipt().toUpperCase());
        }
        dictionary.put(ReportConstants.BL_COMMENTS, StringUtility.toUpperCase(hblDataDto.getBlComments()));
        dictionary.put(ReportConstants.BL_WITH_UNDERSCORE_COMMENTS, StringUtility.toUpperCase(hblDataDto.getBlComments()));
        dictionary.put(ReportConstants.BL_DELIVERY_AGENT, hblDataDto.getDeliveryAgent());
        dictionary.put(ReportConstants.BL_DELIVERY_AGENT_ADDRESS, hblDataDto.getDeliveryAgentAddress());
        dictionary.put(ReportConstants.BL_CARGO_TERMS_DESCRIPTION, StringUtility.toUpperCase(hblDataDto.getCargoTermsDescription()));
        dictionary.put(ReportConstants.BL_REMARKS_DESCRIPTION, StringUtility.toUpperCase(hblDataDto.getBlRemarksDescription()));
        dictionary.put(ReportConstants.CARGO_GROSS_VOLUME_WITH_COMMA, convertToVolumeNumberFormat(hblDataDto.getCargoGrossVolume(), v1TenantSettingsResponse));
        populateBlTransportSectionDetails(hblDataDto, dictionary);
    }

    public void populateBlTransportSectionDetails(HblDataDto hblDataDto, Map<String, Object> dictionary) {

        if (Objects.isNull(hblDataDto)) return;

        if (Objects.nonNull(hblDataDto.getPlaceOfDelivery())) {
            dictionary.put(BL_PLACE_OF_DELIVERY, StringUtility.toUpperCase(hblDataDto.getPlaceOfDelivery()));
        }
        if (Objects.nonNull(hblDataDto.getPortOfLoad())) {
            dictionary.put(BL_PORT_OF_LOADING, StringUtility.toUpperCase(hblDataDto.getPortOfLoad()));
        }
        if (Objects.nonNull(hblDataDto.getPortOfDischarge())) {
            dictionary.put(BL_PORT_OF_DISCHARGE, StringUtility.toUpperCase(hblDataDto.getPortOfDischarge()));
        }
        if (Objects.nonNull(hblDataDto.getPlaceOfReceipt())) {
            dictionary.put(BL_PLACE_OF_RECEIPT, StringUtility.toUpperCase(hblDataDto.getPlaceOfReceipt()));
        }
    }

    public String processBLTransportDetailsFromShipmentModel(String blTransportDetail) {
        if (Objects.isNull(blTransportDetail)) return "";

        if (Boolean.TRUE.equals(commonUtils.getShipmentSettingFromContext().getIsRunnerV3Enabled())) {
            if (blTransportDetail.length() >= 2) {
                String countryCode = blTransportDetail.substring(0, 2);
                if (US.equalsIgnoreCase(countryCode)) {
                    return (countryCode + "," + blTransportDetail.substring(2));
                }
            }
            return blTransportDetail;
        }
        return blTransportDetail;
    }


    private List<String> getNotifyParty(Hbl hbl, ShipmentSettingsDetails shipmentSettingsDetails) {
        List<String> notify = new ArrayList<>();
        if(hbl.getHblNotifyParty() != null && !hbl.getHblNotifyParty().isEmpty()) {
            HblPartyDto hblNotify = hbl.getHblNotifyParty().get(0);
            if(Boolean.TRUE.equals(shipmentSettingsDetails.getDisableBlPartiesName()))
                notify = ReportHelper.getOrgAddress(null, hblNotify.getAddress(), null, null, hblNotify.getEmail(), null);
            else
                notify = ReportHelper.getOrgAddress(hblNotify.getName(), hblNotify.getAddress(), null, null, hblNotify.getEmail(), null);
        }
        return notify;
    }

    private void processConsignorConsigneeTags(Map<String, Object> dictionary, ShipmentSettingsDetails shipmentSettingsDetails, HblDataDto hblDataDto) {
        List<String> consignor;
        List<String> consignee;
        if(Boolean.TRUE.equals(shipmentSettingsDetails.getDisableBlPartiesName())) {
            consignor = ReportHelper.getOrgAddress(null, hblDataDto != null ? hblDataDto.getConsignorAddress() : null, null, null, null, null);
            consignee = ReportHelper.getOrgAddress(null, hblDataDto != null ? hblDataDto.getConsigneeAddress() : null, null, null, null, null);
        } else {
            consignor = ReportHelper.getOrgAddress(hblDataDto != null ? hblDataDto.getConsignorName() : null, hblDataDto != null ? hblDataDto.getConsignorAddress() : null, null, null, null, null);
            consignee = ReportHelper.getOrgAddress(hblDataDto != null ? hblDataDto.getConsigneeName() : null, hblDataDto != null ? hblDataDto.getConsigneeAddress() : null, null, null, null, null);
        }
        addConsignorConsigneeTag(dictionary, hblDataDto, consignor, consignee);
        dictionary.put(ReportConstants.CONSIGNER_CAPS, consignor.stream().map(String::toUpperCase).toList());
        dictionary.put(ReportConstants.CONSIGNEE_CAPS, consignee.stream().map(String::toUpperCase).toList());
    }

    private void addConsignorConsigneeTag(Map<String, Object> dictionary, HblDataDto hblDataDto, List<String> consignor, List<String> consignee) {
        if(!consignor.isEmpty()) {
            dictionary.put(ReportConstants.CONSIGNER, consignor);
            dictionary.put(ReportConstants.CONSIGNER_NAME_FREETEXT_INCAPS, StringUtility.toUpperCase(hblDataDto.getConsignorName()));
        }
        if(!consignee.isEmpty()) {
            dictionary.put(ReportConstants.CONSIGNEE, consignee);
            dictionary.put(ReportConstants.CONSIGNEE_NAME_FREE_TEXT, consignee.stream().map(StringUtility::toUpperCase).toList());
            dictionary.put(CONSIGNEE_NAME_FREETEST_INCAPS, StringUtility.toUpperCase(hblDataDto.getConsigneeName()));
        }
    }

    public void populateUserFields(UsersDto user, Map<String, Object> dictionary) {
        if (user == null) {
            return;
        }
        dictionary.put(USER_DISPLAY_NAME, user.DisplayName);
        dictionary.put(ReportConstants.USER_FULLNAME, user.DisplayName);
        dictionary.put(ReportConstants.TENANT_NAME, user.TenantDisplayName);
        dictionary.put(ReportConstants.USER_NAME, user.Username);
        dictionary.put(PRINT_USER, user.Username.toUpperCase());
        dictionary.put(ReportConstants.USER_EMAIL, user.Email);
        dictionary.put(USER_PHONE_NUMBER, user.Phone);
        dictionary.put(ReportConstants.TENANT_CURRENCY, user.CompanyCurrency);
    }

    public MasterData getMasterListData(MasterDataType type, String itemValue) {
        return masterDataUtils.getMasterListData(type, itemValue);
    }
    public CarrierMasterData getCarrier(String carrier) {
        if(StringUtility.isEmpty(carrier)) return null;
        List<Object> carrierCriteria = Arrays.asList(
                List.of(MasterDataConstants.ITEM_VALUE),
                "=",
                carrier
        );
        CommonV1ListRequest carrierRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(carrierCriteria).build();
        CarrierListObject carrierListObject = new CarrierListObject();
        carrierListObject.setListObject(carrierRequest);
        carrierListObject.setIsList(true);
        Object carrierResponse = masterDataFactory.getMasterDataService().fetchCarrierMasterData(carrierListObject).getData();
        List<CarrierMasterData> carrierMasterData = jsonHelper.convertValueToList(carrierResponse, CarrierMasterData.class);
        if(carrierMasterData == null || carrierMasterData.isEmpty())
            return null;
        return carrierMasterData.get(0);
    }

    public List<ContainerCountByCode> getCountByContainerTypeCode(List<ShipmentContainers> commonContainers) {
        Map<String, Long> containerTypeCountMap = new HashMap<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        if (commonContainers != null) {
            for(ShipmentContainers container : commonContainers) {
                if (container.ContainerTypeCode != null) {
                    Long containerCount = container.getContainerCount();
                    if (!containerTypeCountMap.containsKey(container.ContainerTypeCode)) {
                        containerTypeCountMap.put(container.ContainerTypeCode, containerCount);
                        continue;
                    }
                    Long containers = containerTypeCountMap.get(container.ContainerTypeCode);
                    containers += containerCount;
                    containerTypeCountMap.put(container.ContainerTypeCode, containers);
                }
            }
        }

        List<ContainerCountByCode> containerCountByCode = new ArrayList<>();
        for (var entry : containerTypeCountMap.entrySet()) {
            ContainerCountByCode countByCode = new ContainerCountByCode();
            countByCode.ContainerTypeCode = entry.getKey();
            countByCode.ContainerCount = getDPWWeightVolumeFormat(BigDecimal.valueOf(entry.getValue()), 0, v1TenantSettingsResponse);
            containerCountByCode.add(countByCode);
        }
        return containerCountByCode;
    }

    public List<ContainerCountByCode> getCountByCommonContainerTypeCode(List<ContainerModel> commonContainers) {
        Map<String, Long> containerTypeCountMap = new HashMap<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        if (commonContainers != null) {
            for (var container : commonContainers) {
                if (container.getContainerCode() != null) {
                    Long containerCount = container.getContainerCount();
                    if (!containerTypeCountMap.containsKey(container.getContainerCode())) {
                        containerTypeCountMap.put(container.getContainerCode(), containerCount);
                        continue;
                    }
                    Long containers = containerTypeCountMap.get(container.getContainerCode());
                    containers += containerCount;
                    containerTypeCountMap.put(container.getContainerCode(), containers);
                }
            }
        }

        List<ContainerCountByCode> containerCountByCode = new ArrayList<>();
        for (var entry : containerTypeCountMap.entrySet()) {
            ContainerCountByCode countByCode = new ContainerCountByCode();
            countByCode.ContainerTypeCode = entry.getKey();
            countByCode.ContainerCount = getDPWWeightVolumeFormat(BigDecimal.valueOf(entry.getValue()), 0, v1TenantSettingsResponse);
            containerCountByCode.add(countByCode);
        }
        return containerCountByCode;
    }

    public Awb getHawb(Long id) {
        List<Awb> awb = awbDao.findByShipmentId(id);
        if (awb != null && !awb.isEmpty())
            return awb.get(0);
        return null;
    }

    public Awb getMawb(Long id, boolean withPacks) {
        List<Awb> awb = awbDao.findByConsolidationId(id);
        if(awb != null && !awb.isEmpty()) {
            if(withPacks) {
                awbService.getMawnLinkPacks(awb.get(0), false, null);
            }
            return awb.get(0);
        }
        return null;
    }

    /**
     * Formats and returns a list of address details in a structured line-by-line format.
     * <p>
     * The returned list will include:
     * <ul>
     *   <li>Name</li>
     *   <li>Address line 1</li>
     *   <li>Address line 2 (if present)</li>
     *   <li>A single line combining state, city, and country (in that order, separated by commas)</li>
     *   <li>Zip code (if present)</li>
     *   <li>Phone number (if present)</li>
     * </ul>
     * <p>
     * Example output:
     * <pre>
     * John Doe
     * 123 Main Street
     * Apt 4B
     * California, Los Angeles, USA
     * 90001
     * +1-123-456-7890
     * </pre>
     *
     * @param name     Name of the person or entity
     * @param address1 Primary address line (required if name is empty)
     * @param address2 Secondary address line (optional)
     * @param country  Country name or code (optional)
     * @param state    State or region (optional)
     * @param city     City (optional)
     * @param zipCode  Postal or ZIP code (optional)
     * @param phone    Phone number (optional)
     * @return A list of formatted address lines or {@code null} if both name and address1 are empty
     */
    public static List<String> getFormattedDetails(String name, String address1,String address2, String country, String state, String city, String zipCode, String phone)
    {
        if(StringUtility.isEmpty(name) && StringUtility.isEmpty(address1)) {
            return null;
        }
        List<String> details = new ArrayList<>();
        details.add(name);
        if(!StringUtility.isEmpty(address2)) {
            details.addAll(Arrays.asList(address1, address2));
        }else{
            details.addAll(Arrays.asList(address1));
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
        if(!tempAddress.isEmpty())
            details.add(tempAddress.toString());
        if (!Strings.isNullOrEmpty(zipCode)) details.add(zipCode);
        if (!Strings.isNullOrEmpty(phone)) details.add(phone);
        return details;
    }

    /**
     * Formats and returns a list of address details in a human-readable structure.
     *
     * <p><strong>Output Format:</strong></p>
     * <pre>
     * Name
     * Address1
     * Address2            (only if present)
     * City, State, Zip, Country
     * Phone               (only if present)
     * </pre>
     *
     * @param name      the name or party name (mandatory if address1 is empty)
     * @param address1  the first line of address (mandatory if name is empty)
     * @param address2  the second line of address (optional)
     * @param country   the country (optional)
     * @param state     the state or region (optional)
     * @param city      the city (optional)
     * @param zipCode   the zip/postal code (optional)
     * @param phone     the contact phone number (optional)
     * @return List of formatted address lines, or {@code null} if both name and address1 are empty
     */
    public static List<String> getFormattedDetailsV2(String name, String address1, String address2,
            String country, String state, String city,
            String zipCode, String phone) {

        if (StringUtility.isEmpty(name) && StringUtility.isEmpty(address1)) {
            return null;
        }

        List<String> details = new ArrayList<>();

        // Add name and primary address line
        details.add(name);
        details.add(address1);

        // Add secondary address line if present
        if (!StringUtility.isEmpty(address2)) {
            details.add(address2);
        }

        // Compose the city/state/zip/country line
        StringBuilder locationLine = new StringBuilder();

        if (!Strings.isNullOrEmpty(city)) {
            locationLine.append(city);
        }

        if (!Strings.isNullOrEmpty(state)) {
            if (!locationLine.isEmpty()) locationLine.append(", ");
            locationLine.append(state);
        }

        if (!Strings.isNullOrEmpty(zipCode)) {
            if (!locationLine.isEmpty()) locationLine.append(", ");
            locationLine.append(zipCode);
        }

        if (!Strings.isNullOrEmpty(country)) {
            if (!locationLine.isEmpty()) locationLine.append(", ");
            locationLine.append(country);
        }

        if (!locationLine.isEmpty()) {
            details.add(locationLine.toString());
        }

        // Add phone if present
        if (!Strings.isNullOrEmpty(phone)) {
            details.add(phone);
        }

        return details;
    }


    /**
     Added this method to change the Address format of HAWB and MAWB reports without disturbing the other reports
     */
    public static List<String> getAwbFormattedDetails(String name, String address1, String address2, String city, String state, String zipCode, String country, String contactName, String phone, String taxRegistrationNumber) {
        return getAwbFormattedDetails(name, address1, address2, city, state, zipCode, country, contactName, phone, taxRegistrationNumber, false);
    }
    public static List<String> getAwbFormattedDetails(String name, String address1, String address2, String city, String state, String zipCode, String country, String contactName, String phone, String taxRegistrationNumber, boolean addStringPHReqd)
    {
        List<String> details = new ArrayList<>();
        if(!Strings.isNullOrEmpty(name)){
            details.add(name);
        }
        if(!Strings.isNullOrEmpty(address1)){
            details.add(address1);
        }
        if(!Strings.isNullOrEmpty(address2)){
            details.add(address2);
        }
        StringBuilder tempAddress = new StringBuilder();
        if (!Strings.isNullOrEmpty(city)){
            tempAddress.append(city);
        }
        if (!Strings.isNullOrEmpty(state)){
            if(!tempAddress.isEmpty())
                tempAddress.append(", ");
            tempAddress.append(state);
        }
        if (!Strings.isNullOrEmpty(zipCode)) {
            if(!tempAddress.isEmpty())
                tempAddress.append(", ");
            tempAddress.append(zipCode);
        }
        if (!Strings.isNullOrEmpty(country)){
            if(!tempAddress.isEmpty())
                tempAddress.append(", ");
            tempAddress.append(country);
        }
        if(!tempAddress.isEmpty())
            details.add(tempAddress.toString());
        addContactAndPhoneDetails(contactName, phone, addStringPHReqd, details);
        if (!Strings.isNullOrEmpty(taxRegistrationNumber)) details.add(taxRegistrationNumber);
        return details;
    }

    public static String formattedOrgAddress(
            String name,
            String addressLine1,
            String addressLine2,
            String city,
            String state,
            String zipcode,
            String country
    ) {
        StringBuilder sb = new StringBuilder();

        if (!CommonUtils.isStringNullOrEmpty(name)) {
            sb.append(name.trim()).append("\n");
        }

        if (!CommonUtils.isStringNullOrEmpty(addressLine1)) {
            sb.append(addressLine1.trim()).append("\n");
        }

        if (!CommonUtils.isStringNullOrEmpty(addressLine2)) {
            sb.append(addressLine2.trim()).append("\n");
        }

        // Combine city, state, zipcode, and country into one line
        StringBuilder lastLine = new StringBuilder();
        if (!CommonUtils.isStringNullOrEmpty(city)) {
            lastLine.append(city.trim());
        }
        if (!CommonUtils.isStringNullOrEmpty(state)) {
            if (!lastLine.isEmpty()) lastLine.append(" ");
            lastLine.append(state.trim());
        }
        if (!CommonUtils.isStringNullOrEmpty(zipcode)) {
            if (!lastLine.isEmpty()) lastLine.append(" ");
            lastLine.append(zipcode.trim());
        }
        if (!CommonUtils.isStringNullOrEmpty(country)) {
            if (!lastLine.isEmpty()) lastLine.append(" ");
            lastLine.append(country.trim());
        }

        if (!lastLine.isEmpty()) {
            sb.append(lastLine.toString());
        }

        return sb.toString().trim(); // Remove any trailing newlines
    }

    private static void addContactAndPhoneDetails(String contactName, String phone, boolean addStringPHReqd, List<String> details) {
        StringBuilder contactAndPhoneDetails = new StringBuilder();
        if (!Strings.isNullOrEmpty(contactName)) {
            contactAndPhoneDetails.append(contactName);
        }
        if (!Strings.isNullOrEmpty(phone)) {
            if(!contactAndPhoneDetails.isEmpty())
                contactAndPhoneDetails.append(", ");
            String ph = phone;
            if(addStringPHReqd)
                ph = "PH: " + phone;
            contactAndPhoneDetails.append(ph);
        }
        if (!contactAndPhoneDetails.isEmpty()) {
            details.add(contactAndPhoneDetails.toString());
        }
    }

    private static List<String> addEmailAndPhoneDetails(String email, String emailHeader, String phone, String phoneHeader) {
        List<String> result = new ArrayList<>();

        if (!Strings.isNullOrEmpty(email)) {
            result.add(Strings.isNullOrEmpty(emailHeader) ? email : emailHeader + email);
        }

        if (!Strings.isNullOrEmpty(phone)) {
            if (!result.isEmpty()) {
                result.add(", ");
            }
            result.add(Strings.isNullOrEmpty(phoneHeader) ? phone : phoneHeader + phone);
        }

        return result;
    }

    public static String addCommas(BigDecimal amount)
    {
        if (amount == null) return null;
        DecimalFormat decimalFormat = new DecimalFormat("#,##0.00");
        return decimalFormat.format(amount);
    }

    public static String addCommas(Integer amount)
    {
        if (amount == null) return null;
        DecimalFormat decimalFormat = new DecimalFormat("#,##0.00");
        return decimalFormat.format(amount);
    }

    public static String addCommas(String amount)
    {
        if (amount == null)
        {
            return null;
        }
        try{
            return addCommas(new BigDecimal(amount));
        }
        catch (Exception ex)
        {
            return amount;
        }
    }

    public static String appendZero(String value, int length){
        int size = value.length();
        StringBuilder valueBuilder = new StringBuilder(value);
        for(int i = 0; i < length-size; i++){
            valueBuilder.insert(0, "0");
        }
        value = valueBuilder.toString();
        return value;
    }

    public void jsonDateFormat(Map<String, Object> dictionary) {
        if (dictionary != null) {
            Map<String, Object> dictionaryCopy = new LinkedHashMap<>(dictionary);
            for (Map.Entry<String, Object> entry : dictionaryCopy.entrySet()) {
                Object value = entry.getValue();
                if (value != null && value instanceof LocalDateTime val) {
                    dictionary.put(entry.getKey(), convertToDPWDateFormat(val));
                }
            }
        }
    }

    public static String twoDecimalPlacesFormatDecimal(BigDecimal value)
    {
        if(value == null)
        {
            return "0.00";
        }
        NumberFormat customFormat = NumberFormat.getNumberInstance(Locale.US);
        DecimalFormat customDecimalFormat = (DecimalFormat) customFormat;
        customDecimalFormat.setMaximumFractionDigits(2);
        customDecimalFormat.setMinimumFractionDigits(2);
        return customDecimalFormat.format(value);
    }

    public DateTimeFormatter getDPWDateFormatOrDefault(V1TenantSettingsResponse v1TenantSettingsResponse)
    {
        if(!CommonUtils.isStringNullOrEmpty(v1TenantSettingsResponse.getDPWDateFormat()))
            return DateTimeFormatter.ofPattern(v1TenantSettingsResponse.getDPWDateFormat());
        return DateTimeFormatter.ofPattern(ReportConstants.DPW_DATE_FORMAT_OR_DEFAULT_STRING);
    }

    public static DateTimeFormatter getDPWDateFormatWithTime(String tsDatetimeFormat, boolean withoutSec)
    {
        String timeString;

        if(withoutSec) {
            timeString = "HH:mm";
        } else {
            timeString = "HH:mm:ss";
        }
        if(StringUtility.isNotEmpty(tsDatetimeFormat)) {
            return DateTimeFormatter.ofPattern(tsDatetimeFormat+" " +timeString);
        }
        return DateTimeFormatter.ofPattern(ReportConstants.DPW_DATE_FORMAT_OR_DEFAULT_STRING +" "+timeString);
    }

    public String convertToDPWDateFormat(LocalDateTime date) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        return convertToDPWDateFormat(date, null, v1TenantSettingsResponse);
    }

    public String convertToDPWDateFormat(LocalDateTime date, String tsDatetimeFormat, V1TenantSettingsResponse v1TenantSettingsResponse)
    {
        String strDate = "";
        if (date != null)
        {
            if(!isStringNullOrEmpty(tsDatetimeFormat))
                strDate = date.format(DateTimeFormatter.ofPattern(tsDatetimeFormat));
            else
                strDate = date.format(getDPWDateFormatOrDefault(v1TenantSettingsResponse));
        }
        return strDate;
    }

    public static String convertToDPWDateFormatWithTime(LocalDateTime date, String tsDatetimeFormat, boolean isTimeZone)
    {
        return convertToDPWDateFormatWithTime(date, tsDatetimeFormat, isTimeZone, false);
    }

    public static String convertToDPWDateFormatWithTime(LocalDateTime date, String tsDatetimeFormat, boolean isTimeZone, boolean withoutSec)
    {
        String strDate = "";
        if (date != null)
        {
            if(isTimeZone) {
                strDate = LocalTimeZoneHelper.getDateTime(date).format(getDPWDateFormatWithTime(tsDatetimeFormat, withoutSec));
            } else {
                strDate = date.format(getDPWDateFormatWithTime(tsDatetimeFormat, withoutSec));
            }
        }
        return strDate;
    }

    public String convertToDPWDateFormat(LocalDateTime date, String tsDatetimeFormat, boolean isTimeZone)
    {
        String strDate = "";
        LocalDateTime formatedDate;
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        if (date != null)
        {
            formatedDate = date;
            if(isTimeZone) {
                formatedDate =  LocalTimeZoneHelper.getDateTime(date);
            }
            if(!isStringNullOrEmpty(tsDatetimeFormat))
                strDate = formatedDate.format(DateTimeFormatter.ofPattern(tsDatetimeFormat));
            else
                strDate = formatedDate.format(getDPWDateFormatOrDefault(v1TenantSettingsResponse));
        }
        return strDate.toUpperCase();
    }

    public String convertToWeightNumberFormat(BigDecimal weight) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        return convertToWeightNumberFormat(weight, v1TenantSettingsResponse);
    }

    public String convertToSingleCharWeightFormat(String weightUnit) {
        if(StringUtility.isEmpty(weightUnit)) {
            return "";
        }
        return weightUnit.substring(0, 1).toUpperCase();
    }

    public static String convertToWeightNumberFormat(Object weight, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(weight != null && !CommonUtils.isStringNullOrEmpty(weight.toString())) {
            return convertToWeightNumberFormat(new BigDecimal(weight.toString()), v1TenantSettingsResponse);
        }
        return null;
    }

    public static String convertToWeightNumberFormat(Object weight, int numberDecimalDigits, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(weight != null && !CommonUtils.isStringNullOrEmpty(weight.toString())) {
            return getDPWWeightVolumeFormat(new BigDecimal(weight.toString()), numberDecimalDigits, v1TenantSettingsResponse);
        }
        return null;
    }

    public static String convertToWeightNumberFormat(BigDecimal weight, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(weight != null) {
            int numberDecimalDigits = 0;
            if(v1TenantSettingsResponse.getWeightDecimalPlace() != null)
                numberDecimalDigits = v1TenantSettingsResponse.getWeightDecimalPlace();
            return getDPWWeightVolumeFormat(weight, numberDecimalDigits, v1TenantSettingsResponse);
        }
        return null;
    }

    public String convertToVolumeNumberFormat(BigDecimal volume) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        return convertToVolumeNumberFormat(volume, v1TenantSettingsResponse);
    }
    public static String convertToVolumeNumberFormat(Object volume, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(volume != null && !CommonUtils.isStringNullOrEmpty(volume.toString())) {
            return convertToVolumeNumberFormat(new BigDecimal(volume.toString()), v1TenantSettingsResponse);
        }
        return null;
    }
    public static String convertToVolumeNumberFormat(BigDecimal volume, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(volume != null) {
            int numberDecimalDigits = 0;
            if(v1TenantSettingsResponse.getVolumeDecimalPlace() != null)
                numberDecimalDigits = v1TenantSettingsResponse.getVolumeDecimalPlace();
            return getDPWWeightVolumeFormat(volume, numberDecimalDigits, v1TenantSettingsResponse);
        }
        return null;
    }

    public static String convertToVolumetricWeightFormat(BigDecimal weight, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(weight != null) {
            int numberDecimalDigits = 2;
            if(v1TenantSettingsResponse.getDecimalValueForVolumetricWeight() != null)
                numberDecimalDigits = v1TenantSettingsResponse.getDecimalValueForVolumetricWeight();
            return getDPWWeightVolumeFormat(weight, numberDecimalDigits, v1TenantSettingsResponse);
        }
        return null;
    }

    public static String addCommasWithPrecision(BigDecimal number, int decimalPlaces, boolean strictPrecision) {
        if (number != null) {
            try {
                BigDecimal roundedNumber = number.setScale(decimalPlaces, RoundingMode.HALF_UP);
                DecimalFormat decimalFormat = new DecimalFormat();
                decimalFormat.setMaximumFractionDigits(decimalPlaces);
                if (Boolean.TRUE.equals(strictPrecision))
                    decimalFormat.setMinimumFractionDigits(decimalPlaces);
                return decimalFormat.format(roundedNumber);
            } catch (Exception e) {
                log.error("An error occurred: {}", e.getMessage(), e);
            }
        }
        return String.valueOf(number);
    }

    public static String displayFormat(BigDecimal value, int numberDecimalDigits, V1TenantSettingsResponse v1TenantSettingsResponse, boolean strictPrecision) {
        if(value != null && v1TenantSettingsResponse != null) {
            if(v1TenantSettingsResponse.getCurrencyDigitGrouping() != null) {
                char customThousandsSeparator = ',';
                char customDecimalSeparator = '.';
                if(v1TenantSettingsResponse.getCurrencyGroupingNumber() != null && v1TenantSettingsResponse.getCurrencyGroupingNumber() == GroupingNumber.DotAndComma.getValue()) {
                    customThousandsSeparator = '.';
                    customDecimalSeparator = ',';
                }
                return formatValue(value, customDecimalSeparator, customThousandsSeparator, numberDecimalDigits, v1TenantSettingsResponse.getCurrencyDigitGrouping());
            }
            else {
                return addCommasWithPrecision(value, numberDecimalDigits, strictPrecision);
            }
        }
        if(value != null)
            return value.toString();
        return null;
    }

    public static String getDPWWeightVolumeFormat(BigDecimal value, int numberDecimalDigits, V1TenantSettingsResponse v1TenantSettingsResponse, boolean strictPrecision) {
        if(value != null && v1TenantSettingsResponse != null) {
            if(v1TenantSettingsResponse.getWVDigitGrouping() != null) {
                char customThousandsSeparator = ',';
                char customDecimalSeparator = '.';
                if(v1TenantSettingsResponse.getWVGroupingNumber() != null && v1TenantSettingsResponse.getWVGroupingNumber() == GroupingNumber.DotAndComma.getValue()) {
                    customThousandsSeparator = '.';
                    customDecimalSeparator = ',';
                }
                return formatValue(value, customDecimalSeparator, customThousandsSeparator, numberDecimalDigits, v1TenantSettingsResponse.getWVDigitGrouping());
            }
            else {
                return addCommasWithPrecision(value, numberDecimalDigits, strictPrecision);
            }
        }
        if(value != null)
            return value.toString();
        return null;
    }

    public String roundUpToNextHalf(String input) {
        double value = Double.parseDouble(input);

        // Multiply by 2, apply Math.ceil to round up to nearest 0.5, then divide back
        double rounded = Math.ceil(value * 2) / 2.0;

        // Format to keep 1 decimal place
        DecimalFormat df = new DecimalFormat("#0.0");
        return df.format(rounded);
    }

    public static String getDPWWeightVolumeFormat(BigDecimal value, int numberDecimalDigits, V1TenantSettingsResponse v1TenantSettingsResponse) {
        return getDPWWeightVolumeFormat(value, numberDecimalDigits, v1TenantSettingsResponse, false);
    }

    public static String formatValue(BigDecimal value, char customDecimalSeparator, char customThousandsSeparator, int numberDecimalDigits, Integer digitGrouping) {
        if(value == null)
            return null;
        int dynamicGroupSizes = (digitGrouping == DigitGrouping.THREE.getValue()) ? 3 : 2;

        NumberFormat customFormat = NumberFormat.getNumberInstance(Locale.US);
        DecimalFormat customDecimalFormat = (DecimalFormat) customFormat;

        customDecimalFormat.setDecimalSeparatorAlwaysShown(true);
        customDecimalFormat.setMaximumFractionDigits(numberDecimalDigits);
        customDecimalFormat.setMinimumFractionDigits(numberDecimalDigits);

        DecimalFormatSymbols symbols = customDecimalFormat.getDecimalFormatSymbols();
        symbols.setDecimalSeparator(customDecimalSeparator);
        symbols.setGroupingSeparator(customThousandsSeparator);
        customDecimalFormat.setDecimalFormatSymbols(symbols);

        customDecimalFormat.setGroupingUsed(true);
        customDecimalFormat.setGroupingSize(3);

        String result = customDecimalFormat.format(value);
        if(numberDecimalDigits == 0)
            result = result.substring(0, result.length() - 1);
        if(dynamicGroupSizes == 3)
            return result;
        StringBuilder formattedResult = getFormattedResult(customDecimalSeparator, customThousandsSeparator, numberDecimalDigits, result);
        return formattedResult.toString();
    }

    private static StringBuilder getFormattedResult(char customDecimalSeparator, char customThousandsSeparator, int numberDecimalDigits, String result) {
        StringBuilder formattedResult = new StringBuilder();
        int count = 0;
        boolean threeDigitSeparatorAdded = false;
        boolean decimalPassed = numberDecimalDigits == 0;
        for (int i = result.length() - 1; i >= 0; i--) {
            char c = result.charAt(i);
            if(c != customThousandsSeparator && c != '-') {
                if (c == customDecimalSeparator) {
                    decimalPassed = true;
                } else if (decimalPassed && count == 2 && threeDigitSeparatorAdded) {
                    formattedResult.insert(0, customThousandsSeparator);
                    count = 1;
                } else if(decimalPassed && count == 3 && !threeDigitSeparatorAdded) {
                    threeDigitSeparatorAdded = true;
                    formattedResult.insert(0, customThousandsSeparator);
                    count = 1;
                }
                else if(decimalPassed) {
                    count++;
                }
                formattedResult.insert(0, c);
            }
            if(c == '-')
                formattedResult.insert(0, c);
        }
        return formattedResult;
    }

    public static BigDecimal getRoundedBigDecimal(BigDecimal value, int scale, RoundingMode roundingMode) {
        if(value == null) {
            return null;
        }

        return value.setScale(scale, roundingMode);
    }

    public static String getValueFromMap(Map<String, Object> dataMap, String key) {
        if (Objects.isNull(dataMap))
            return null;

        Object value = dataMap.get(key);
        if(!(value instanceof String)) {
            return null;
        }
        return value.toString();
    }

    public String getPortDetails(String unLocCode) {
        Map<String, UnlocationsResponse> unlocationsMap = new HashMap<>();
        Set<String> locCodes = new HashSet<>();
        locCodes.add(unLocCode);
        Map<String, EntityTransferUnLocations> entityTransferUnLocationsMap = masterDataUtils.getLocationDataFromCache(locCodes, EntityTransferConstants.LOCATION_SERVICE_GUID);
        for (Map.Entry<String, EntityTransferUnLocations> entry : entityTransferUnLocationsMap.entrySet()) {
            String key = entry.getKey();
            UnlocationsResponse value = jsonHelper.convertValue(entry.getValue(), UnlocationsResponse.class);
            unlocationsMap.put(key, value);
        }
        UnlocationsResponse unlocationsResponse = unlocationsMap.get(unLocCode);
        if(unlocationsResponse != null) {
            return combineStringsWithComma(unlocationsResponse.getName(), unlocationsResponse.getCountry());
        }
        return "";
    }

    public UnlocationsResponse getUNLocRow(String unLocCode) {
        return masterDataUtils.getUNLocRow(unLocCode);
    }

    public CommodityResponse getCommodity(String commodityCode) {
        if(commodityCode == null || commodityCode.isEmpty())
            return null;
        List <Object> criteria = Arrays.asList(
                Arrays.asList("Code"),
                "=",
                commodityCode
        );
        CommonV1ListRequest commonV1ListRequest = CommonV1ListRequest.builder().skip(0).criteriaRequests(criteria).build();
        Object commodity = masterDataFactory.getMasterDataService().fetchCommodityData(commonV1ListRequest).getData();
        List<CommodityResponse> commodityResponses = jsonHelper.convertValueToList(commodity, CommodityResponse.class);
        if(!commodityResponses.isEmpty())
            return commodityResponses.get(0);
        return null;
    }


    public List<PackingModel> getAllShipmentsPacks(List<ShipmentModel> shipmentDetails){
        List<PackingModel> packingList = new ArrayList<>();
        if(shipmentDetails != null) {
            for(var shipment : shipmentDetails) {
                packingList.addAll(shipment.getPackingList());
            }
        }
        return packingList;
    }
    public Pair<BigDecimal, String> getTotalWeight(List<PackingModel> packingList) {
        Pair<BigDecimal, String> res = Pair.of(BigDecimal.ZERO, null);
        if(packingList != null ) {
            String weightUnit = null;
            BigDecimal totalWeight = BigDecimal.ZERO;

            for(var packing : packingList) {
                if(packing.getWeight() != null && !isStringNullOrEmpty(packing.getWeightUnit())) {
                    if(weightUnit == null) {
                        weightUnit = packing.getWeightUnit();
                    }
                    if(!Objects.equals(packing.getWeightUnit(), weightUnit))
                        return res;
                    totalWeight = totalWeight.add(packing.getWeight());
                }
            }
            return Pair.of(totalWeight, weightUnit);
        }
        return res;
    }

    public Pair<BigDecimal, String> getTotalVolume(List<PackingModel> packingList) {
        Pair<BigDecimal, String> res = Pair.of(BigDecimal.ZERO, null);
        if(packingList != null ) {
            String volumeUnit = null;
            BigDecimal totalVolume = BigDecimal.ZERO;

            for(var packing : packingList) {
                if(packing.getVolume() != null && !isStringNullOrEmpty(packing.getVolumeUnit())) {
                    if(volumeUnit == null) {
                        volumeUnit = packing.getVolumeUnit();
                    }
                    if(!Objects.equals(packing.getVolumeUnit(), volumeUnit))
                        return res;
                    totalVolume = totalVolume.add(packing.getVolume());
                }
            }
            return Pair.of(totalVolume, volumeUnit);
        }
        return res;
    }

    public List<ShipmentAndContainerResponse> getShipmentAndContainerResponse(List<ShipmentModel> shipments) {
        List<ShipmentAndContainerResponse> shipmentContainers = new ArrayList<>();
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        if(shipments == null)
            return shipmentContainers;

        for(var shipment : shipments) {
            ShipmentAndContainerResponse shipmentContainer = new ShipmentAndContainerResponse();

            shipmentContainer.hsnNumber = shipment.getAdditionalDetails().getHsnNumber() != null ?
                    shipment.getAdditionalDetails().getHsnNumber().toString(): null;
            shipmentContainer.houseBill = shipment.getHouseBill();
            shipmentContainer.masterBill = shipment.getMasterBill();
            shipmentContainer.description = StringUtility.toUpperCase(shipment.getGoodsDescription());
            shipmentContainer.weight = convertToWeightNumberFormat(StringUtility.convertToString(shipment.getWeight()), v1TenantSettingsResponse);
            shipmentContainer.volume = convertToVolumeNumberFormat(StringUtility.convertToString(shipment.getVolume()), v1TenantSettingsResponse);
            shipmentContainer.packs = getDPWWeightVolumeFormat(BigDecimal.valueOf(shipment.getNoOfPacks() != null ? shipment.getNoOfPacks() : 0), 0, v1TenantSettingsResponse);
            shipmentContainer.packsUnit = StringUtility.convertToString(shipment.getPacksUnit());
            shipmentContainer.weightUnit = StringUtility.convertToString(shipment.getWeightUnit());
            shipmentContainer.volumeUnit = StringUtility.convertToString(shipment.getVolumeUnit());
            shipmentContainer.volumeUnitDescription = StringUtility.toUpperCase(shipment.getVolumeUnit());
            shipmentContainer.weightUnitDescription = StringUtility.toUpperCase(shipment.getWeightUnit());
            shipmentContainer.packsUnitDescription = StringUtility.toUpperCase(shipment.getPacksUnit());
            shipmentContainer.marksnNumbers = StringUtility.toUpperCase(shipment.getMarksNum());
            shipmentContainer.freightOverseasCurrency = shipment.getFreightOverseasCurrency();

            addPartyModelDataInResponse(shipment, shipmentContainer);

            shipmentContainer.setShipmentContainers(shipment.getContainersList().stream()
                    .map(i -> jsonHelper.convertValue(i, ShipmentContainers.class))
                    .toList());

            shipmentContainer.setConsigneeAddressFreeText(getPartyAddress(shipment.getConsignee()));
            shipmentContainer.setConsignerAddressFreeText(getPartyAddress(shipment.getConsigner()));
            if (shipment.getAdditionalDetails() != null)
                shipmentContainer.setNotifyPartyAddressFreeText(getPartyAddress(shipment.getAdditionalDetails().getNotifyParty()));

            shipmentContainers.add(shipmentContainer);
        }
        return shipmentContainers;
    }

    private void addPartyModelDataInResponse(ShipmentModel shipment, ShipmentAndContainerResponse shipmentContainer) {
        addConsignorDataInResponse(shipment, shipmentContainer);
        addConsigneeDataInResponse(shipment, shipmentContainer);

        PartiesModel notify = shipment.getAdditionalDetails().getNotifyParty();
        if(notify != null && notify.getAddressData() != null) {
            shipmentContainer.notifyCompanyName = getConsignerCompanyName(notify);
            shipmentContainer.notifyAddress1 = notify.getAddressData().get(ADDRESS1) != null ? notify.getAddressData().get(ADDRESS1).toString() : null;
            shipmentContainer.notifyAddress2 = notify.getAddressData().get(ADDRESS2) != null ? notify.getAddressData().get(ADDRESS2).toString() : null;
            shipmentContainer.notifyCountry = notify.getAddressData().get(COUNTRY) != null ? notify.getAddressData().get(COUNTRY).toString() : null;
            shipmentContainer.notifyZip = notify.getAddressData().get(ZIP_POST_CODE) != null ? notify.getAddressData().get(ZIP_POST_CODE).toString() : null;
            if (notify.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
                List<String> rawData = getAddressList(StringUtility.convertToString(notify.getAddressData().containsKey(PartiesConstants.RAW_DATA)));
                shipmentContainer.setNotifyPartyAddressFreeText(rawData.stream().map(StringUtility::toUpperCase).toList());
            }
        }
    }

    private void addConsigneeDataInResponse(ShipmentModel shipment, ShipmentAndContainerResponse shipmentContainer) {
        PartiesModel consignee = shipment.getConsignee();
        if(consignee != null && consignee.getAddressData() != null) {
            shipmentContainer.consigneeCompanyName = consignee.getAddressData().get(COMPANY_NAME) != null ? StringUtility.toUpperCase(consignee.getAddressData().get(COMPANY_NAME).toString()) : null;
            shipmentContainer.consigneeAddress1 = consignee.getAddressData().get(ADDRESS1) != null ? consignee.getAddressData().get(ADDRESS1).toString() : null;
            shipmentContainer.consigneeAddress2 = consignee.getAddressData().get(ADDRESS2) != null ? consignee.getAddressData().get(ADDRESS2).toString() : null;
            shipmentContainer.consigneeCountry = consignee.getAddressData().get(COUNTRY) != null ? consignee.getAddressData().get(COUNTRY).toString() : null;
            shipmentContainer.consigneeZip = consignee.getAddressData().get(ZIP_POST_CODE) != null ? consignee.getAddressData().get(ZIP_POST_CODE).toString() : null;
            if (consignee.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
                List<String> rawData = getAddressList(StringUtility.convertToString(consignee.getAddressData().containsKey(PartiesConstants.RAW_DATA)));
                shipmentContainer.setConsigneeAddressFreeText(rawData.stream().map(StringUtility::toUpperCase).toList());
            }
        }
    }

    private void addConsignorDataInResponse(ShipmentModel shipment, ShipmentAndContainerResponse shipmentContainer) {
        PartiesModel consigner = shipment.getConsigner();
        if(consigner != null && consigner.getAddressData() != null) {

            shipmentContainer.consignerCompanyName = consigner.getAddressData().get(COMPANY_NAME) != null ? StringUtility.toUpperCase(consigner.getAddressData().get(COMPANY_NAME).toString()) : null;
            shipmentContainer.consignerAddress1 = consigner.getAddressData().get(ADDRESS1) != null ? consigner.getAddressData().get(ADDRESS1).toString() : null;
            shipmentContainer.consignerAddress2 = consigner.getAddressData().get(ADDRESS2) != null ? consigner.getAddressData().get(ADDRESS2).toString() : null;
            shipmentContainer.consignerCountry = consigner.getAddressData().get(COUNTRY) != null ? consigner.getAddressData().get(COUNTRY).toString() : null;
            shipmentContainer.consignerZip = consigner.getAddressData().get(ZIP_POST_CODE) != null ? consigner.getAddressData().get(ZIP_POST_CODE).toString() : null;
            if (consigner.getAddressData().containsKey(PartiesConstants.RAW_DATA)) {
                List<String> rawData = getAddressList(StringUtility.convertToString(consigner.getAddressData().containsKey(PartiesConstants.RAW_DATA)));
                shipmentContainer.setConsignerAddressFreeText(rawData.stream().map(StringUtility::toUpperCase).toList());
            }
        }
    }

    public List<ShipmentResponse> getShipmentResponse(List<ShipmentModel> shipments) {
        List<ShipmentResponse> shipmentResponses = new ArrayList<>();
        if (shipments == null) {
            return shipmentResponses;
        }

        for(var shipment : shipments) {
            ShipmentResponse response = new ShipmentResponse();
            PartiesModel consigner = shipment.getConsigner();
            PartiesModel consignee = shipment.getConsignee();

            response.houseBill = shipment.getHouseBill();
            response.masterBill = shipment.getMasterBill();
            if(consigner != null && consigner.getAddressData() != null) {
                response.consignerCompanyName = getConsignerCompanyName(consigner);
                response.consignerLocalName = getLocalName(consigner);
            }
            if(consignee != null && consignee.getAddressData() != null) {
                response.consigneeCompanyName = getConsignerCompanyName(consignee);
                response.consigneeLocalName = getLocalName(consignee);
            }
            response.weight = shipment.getWeight();
            response.weightUnit = shipment.getWeightUnit();
            response.setConsigner(getPartyAddress(shipment.getConsigner()));
            response.setConsignee(getPartyAddress(shipment.getConsignee()));

            response.setConsigneeAddressFreeText(getPartyAddress(shipment.getConsigner()));
            response.setConsignerAddressFreeText(getPartyAddress(shipment.getConsignee()));
            response.setNotifyPartyAddressFreeText(getPartyAddress(shipment.getAdditionalDetails().getNotifyParty()));
            response.description = StringUtility.toUpperCase(shipment.getGoodsDescription());

            response.hsnNumber = getHsnNumber(shipment);
            response.totalPacks = getTotalPacks(shipment);
            response.freightOverseasCurrency = shipment.getFreightOverseasCurrency();

            shipmentResponses.add(response);
        }
        return shipmentResponses;
    }

    private String getLocalName(PartiesModel consigner) {
        return consigner.getAddressData().get(ReportConstants.LOCAL_NAME) != null ? consigner.getAddressData().get(ReportConstants.LOCAL_NAME).toString() : null;
    }

    private String getConsignerCompanyName(PartiesModel consigner) {
        return consigner.getAddressData().get(COMPANY_NAME) != null ? consigner.getAddressData().get(COMPANY_NAME).toString() : null;
    }

    private String getHsnNumber(ShipmentModel shipment) {
        return shipment.getAdditionalDetails().getHsnNumber() != null ? shipment.getAdditionalDetails().getHsnNumber().toString() : null;
    }

    public static Long getTotalPacks(ShipmentModel shipmentDetails){
        long sum = 0L;
        if (shipmentDetails == null || shipmentDetails.getPackingList() == null)
            return sum;
        for (var packs : shipmentDetails.getPackingList()) {
            sum += Long.parseLong(packs.getPacks());
        }
        return sum;
    }


    public static List<String> getPartyAddress(PartiesModel party) {
        if(party != null && party.getAddressData() != null) {
            return ReportHelper.getOrgAddress(
                    getValueFromMap(party.getAddressData(),COMPANY_NAME),
                    getValueFromMap(party.getAddressData(),ADDRESS1),
                    getValueFromMap(party.getAddressData(),ADDRESS2),
                    ReportHelper.getCityCountry(
                            getValueFromMap(party.getAddressData(),CITY),
                            getValueFromMap(party.getAddressData(),COUNTRY)
                    ),
                    getValueFromMap(party.getAddressData(),ZIP_POST_CODE),
                    getValueFromMap(party.getAddressData(),STATE)
            );
        }

        return List.of();
    }
    private boolean isStringNullOrEmpty(String s){
        return s == null || s.isEmpty();
    }

    public static <T> boolean listIsNullOrEmpty(List<T> list) {
        return list == null || list.isEmpty();
    }

    public Boolean getIsHbl(ShipmentModel shipmentModel) {
        if(shipmentModel.getTransportMode().equalsIgnoreCase(Constants.TRANSPORT_MODE_AIR)) {
            if(shipmentModel.getDirection().equalsIgnoreCase(EXP)) {
                List<Awb> awbList = awbDao.findByShipmentIdByQuery(shipmentModel.getId());
                String entityType = (Objects.equals(shipmentModel.getJobType(), Constants.SHIPMENT_TYPE_DRT)) ? Constants.DMAWB : Constants.HAWB;
                if (awbList != null && !awbList.isEmpty() && awbList.get(0).getAwbShipmentInfo().getEntityType().equalsIgnoreCase(entityType))
                        return false;
            }
            return false;
        } else if (shipmentModel.getDirection().equalsIgnoreCase(EXP)) {
            return true;
        }
        return false;
    }

    public List<String> createUnLocoRequestFromShipmentModel(ShipmentModel shipmentModel) {
        List<String> request = new ArrayList<>();
        if (Objects.isNull(shipmentModel)) return request;

        if (!Objects.isNull(shipmentModel.getAdditionalDetails())) {
            request.add(shipmentModel.getAdditionalDetails().getPaidPlace());
            request.add(shipmentModel.getAdditionalDetails().getPlaceOfIssue());
            request.add(shipmentModel.getAdditionalDetails().getPlaceOfSupply());
        }

        if (!Objects.isNull(shipmentModel.getCarrierDetails())) {
            request.add(shipmentModel.getCarrierDetails().getOriginPort());
            request.add(shipmentModel.getCarrierDetails().getDestinationPort());
            request.add(shipmentModel.getCarrierDetails().getOrigin());
            request.add(shipmentModel.getCarrierDetails().getDestination());
        }
        return request.stream().filter(Objects::nonNull).toList();
    }

    public List<String> createUnLocoRequestFromConsolidation(ConsolidationModel consolidationModel) {
        List<String> request = new ArrayList<>();
        if (Objects.isNull(consolidationModel)) return request;

        if (!Objects.isNull(consolidationModel.getCarrierDetails())) {
            request.add(consolidationModel.getCarrierDetails().getOriginPort());
            request.add(consolidationModel.getCarrierDetails().getDestinationPort());
            request.add(consolidationModel.getCarrierDetails().getOrigin());
            request.add(consolidationModel.getCarrierDetails().getDestination());
        }
        return request.stream().filter(Objects::nonNull).toList();
    }

    public List<MasterListRequest> createMasterListsRequestFromShipment(ShipmentModel shipmentModel) {
        List<MasterListRequest> request = new ArrayList<>();
        if (Objects.isNull(shipmentModel)) return request;
        request.add(createMasterListRequest(MasterDataType.PAYMENT, shipmentModel.getPaymentTerms()));
        request.add(createMasterListRequest(MasterDataType.SERVICE_MODE, shipmentModel.getServiceType()));
        request.add(createMasterListRequest(MasterDataType.TRANSPORT_MODE, shipmentModel.getTransportMode()));
        request.add(createMasterListRequest(MasterDataType.CUSTOM_SHIPMENT_TYPE, shipmentModel.getDirection()));
        request.add(createMasterListRequest(MasterDataType.PACKS_UNIT, shipmentModel.getPacksUnit()));
        request.add(createMasterListRequest(MasterDataType.VOLUME_UNIT, shipmentModel.getVolumeUnit()));
        request.add(createMasterListRequest(MasterDataType.WEIGHT_UNIT, shipmentModel.getNetWeightUnit()));
        if (!Objects.isNull(shipmentModel.getAdditionalDetails())) {
            request.add(createMasterListRequest(MasterDataType.RELEASE_TYPE, shipmentModel.getAdditionalDetails().getReleaseType()));
        }
        return request;
    }

    public List<MasterListRequest> createMasterListsRequestFromConsole(ConsolidationModel consolidationModel) {
        List<MasterListRequest> request = new ArrayList<>();
        if (Objects.isNull(consolidationModel)) return request;
        request.add(createMasterListRequest(MasterDataType.PAYMENT, consolidationModel.getPayment()));
        if (consolidationModel.getAllocations() != null) {
            request.add(createMasterListRequest(MasterDataType.VOLUME_UNIT, consolidationModel.getAllocations().getVolumeUnit()));
            request.add(createMasterListRequest(MasterDataType.WEIGHT_UNIT, consolidationModel.getAllocations().getWeightUnit()));
        }
        return request;
    }

    public List<MasterListRequest> createMasterListsRequestFromHbl(Hbl hbl) {
        List<MasterListRequest> request = new ArrayList<>();
        if (Objects.isNull(hbl)) return request;
        request.add(createMasterListRequest(MasterDataType.VOLUME_UNIT, hbl.getHblData().getCargoGrossVolumeUnit()));
        request.add(createMasterListRequest(MasterDataType.WEIGHT_UNIT, hbl.getHblData().getCargoGrossWeightUnit()));
        return request;
    }
    public List<MasterListRequest> createMasterListsRequestFromUnLocoMap(Map<String, UnlocationsResponse> unlocationsMap) {
        List<MasterListRequest> request = new ArrayList<>();
        for (String key : unlocationsMap.keySet())
            request.add(createMasterListRequest(MasterDataType.COUNTRIES, unlocationsMap.get(key).getCountry()));
        return request;
    }


    public MasterListRequest createMasterListRequest (MasterDataType itemType, String itemValue) {
        if (StringUtility.isEmpty(itemValue)) return null;
        return MasterListRequest.builder().ItemType(itemType.getDescription()).ItemValue(itemValue).build();
    }

    public Map<Integer, Map<String, MasterData>> fetchInBulkMasterList(MasterListRequestV2 requests) {
        Map<Integer, Map<String, MasterData>> dataMap = new HashMap<>();
        if(requests.getMasterListRequests() != null && !requests.getMasterListRequests().isEmpty()) {
            V1DataResponse response = v1Service.fetchMultipleMasterData(requests);
            List<MasterData> masterLists = jsonHelper.convertValueToList(response.entities, MasterData.class);
            masterLists.forEach(masterData -> {
                dataMap.putIfAbsent(masterData.getItemType(), new HashMap<>());
                dataMap.get(masterData.getItemType()).put(masterData.getItemValue(), masterData);
            });
        }
        return dataMap;
    }


    public void populateHasContainerFields(ShipmentModel shipmentModel, Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if ((!dictionary.containsKey(SHIPMENT_CONTAINERS) || dictionary.get(SHIPMENT_CONTAINERS) == null) && shipmentModel.getContainersList() != null && !shipmentModel.getContainersList().isEmpty()) {
            dictionary.put(ReportConstants.SHIPMENT_PACKING_HAS_CONTAINERS, true);
            List<Map<String, Object>> valuesContainer = new ArrayList<>();
            for (ContainerModel containerModel : shipmentModel.getContainersList()) {
                ShipmentContainers shipmentContainers = getShipmentContainer(containerModel);
                String shipContJsonString = jsonHelper.convertToJson(shipmentContainers);
                Map<String, Object> shipContJson = jsonHelper.convertJsonToMap(shipContJsonString);
                updateShipContJson(v1TenantSettingsResponse, shipContJson);
                valuesContainer.add(shipContJson);
            }
            dictionary.put(ReportConstants.SHIPMENT_CONTAINERS, valuesContainer);
        }
        else {
            dictionary.put(ReportConstants.SHIPMENT_PACKING_HAS_CONTAINERS, false);
        }
    }

    public void populateV3TruckDriverDetailsTags(ShipmentModel shipmentModel, Map<String, Object> dictionary) {
        if(shipmentModel.getTruckDriverDetails() != null && !shipmentModel.getTruckDriverDetails().isEmpty()) {
            TruckDriverDetailsModel truckDriverDetailsModel = shipmentModel.getTruckDriverDetails().get(0);
            dictionary.put(S_TRUCK_NO, toUpperCase(truckDriverDetailsModel.getTruckNumberPlate()));
            dictionary.put(S_DRIVER_NAME, toUpperCase(truckDriverDetailsModel.getDriverName()));
            dictionary.put(S_TRAILER_NO, toUpperCase(truckDriverDetailsModel.getTrailerNumberPlate()));
            dictionary.put(S_DRIVER_MOBILE_NUMBER, toUpperCase(truckDriverDetailsModel.getDriverMobileNumber()));
            dictionary.put(S_DRIVER_ID, toUpperCase(truckDriverDetailsModel.getDriverId()));
            dictionary.put(S_TRUCK_TRAILER_TYPE, truckDriverDetailsModel.getTruckOrTrailerType());
            dictionary.put(S_TRANSPORT_BY, toUpperCase(truckDriverDetailsModel.getTransporterType().getDescription()));
            if(truckDriverDetailsModel.getTransporterType().equals(Ownership.Self)) {
                dictionary.put(S_TRANSPORTER_NAME, toUpperCase(truckDriverDetailsModel.getSelfTransporterName()));
            } else {
                try { dictionary.put(S_TRANSPORTER_NAME, toUpperCase((String) truckDriverDetailsModel.getThirdPartyTransporter().getOrgData().get(FULL_NAME))); } catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
            }
        }
    }

    private void updateShipContJson(V1TenantSettingsResponse v1TenantSettingsResponse, Map<String, Object> shipContJson) {
        if(shipContJson.containsKey(ReportConstants.GROSS_VOLUME) && shipContJson.get(ReportConstants.GROSS_VOLUME) != null)
            shipContJson.put(ReportConstants.GROSS_VOLUME, convertToVolumeNumberFormat(shipContJson.get(ReportConstants.GROSS_VOLUME), v1TenantSettingsResponse));
        if (shipContJson.containsKey(ReportConstants.GROSS_WEIGHT) && shipContJson.get(ReportConstants.GROSS_WEIGHT) != null)
            shipContJson.put(ReportConstants.GROSS_WEIGHT, convertToWeightNumberFormat(shipContJson.get(ReportConstants.GROSS_WEIGHT), v1TenantSettingsResponse));
        if (shipContJson.containsKey(ReportConstants.NET_WEIGHT) && shipContJson.get(ReportConstants.NET_WEIGHT) != null)
            shipContJson.put(ReportConstants.NET_WEIGHT, convertToWeightNumberFormat(new BigDecimal(shipContJson.get(ReportConstants.NET_WEIGHT).toString())));
        if (shipContJson.containsKey(MIN_TEMP) && shipContJson.get(MIN_TEMP) != null)
            shipContJson.put(MIN_TEMP, convertToWeightNumberFormat(new BigDecimal(String.valueOf(shipContJson.get(MIN_TEMP)))));
        if (shipContJson.containsKey(MAX_TEMP) && shipContJson.get(MAX_TEMP) != null)
            shipContJson.put(MAX_TEMP, convertToWeightNumberFormat(new BigDecimal(String.valueOf(shipContJson.get(MAX_TEMP)))));
        if(shipContJson.containsKey(NO_OF_PACKAGES) && shipContJson.get(ReportConstants.NO_OF_PACKAGES) != null)
            shipContJson.put(ReportConstants.NO_OF_PACKAGES, getDPWWeightVolumeFormat(new BigDecimal(String.valueOf(shipContJson.get(NO_OF_PACKAGES))), 0, v1TenantSettingsResponse));
    }

    // Populates packing details fields in the source dictionary
    // can return List<Map<String, Object>> packing Dictionary, keeping it void for now
    @SuppressWarnings("java:S3516")
    public List<Map<String, Object>> getPackingDetails(ShipmentModel shipment, Map<String, Object> dictionary) {
        if(shipment.getPackingList() == null || shipment.getPackingList().isEmpty()) {
            dictionary.put(HAS_PACK_DETAILS, false);
            return null;
        }

        List<Map<String, Object>> packsDictionary = new ArrayList<>();
        Map<String, EntityTransferCommodityType> commodityTypeMap = getCommodityTypeMap(shipment);
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        groupPacksDetails(shipment.getPackingList(), v1TenantSettingsResponse, dictionary, shipment.getContainersList());
        dictionary.put(SHIPMENT_PACKS, shipment.getPackingList());
        for(var pack : shipment.getPackingList()) {
            packsDictionary.add(processPackDetails(pack, shipment, v1TenantSettingsResponse, commodityTypeMap));
        }

        dictionary.put(HAS_PACK_DETAILS, true);
        dictionary.put(PACKS_DETAILS, packsDictionary);
        return packsDictionary;
    }

    private Map<String, Object> processPackDetails(PackingModel pack, ShipmentModel shipment, V1TenantSettingsResponse v1TenantSettingsResponse,Map<String, EntityTransferCommodityType> commodityTypeMap) {
        Map<String, Object> dict = new HashMap<>();
        processStringUtilityTags(pack, dict);
        if(Boolean.TRUE.equals(pack.getMarinePollutant()))
            dict.put(MARINE_POLLUTANT, "Marine Pollutant");
        processPackCommodity(pack, dict, commodityTypeMap);
        processPackingWeightVolume(pack, dict, v1TenantSettingsResponse);
        if (shipment.getPickupDetails() != null && shipment.getPickupDetails().getActualPickupOrDelivery() != null) {
            dict.put(LOADED_DATE, convertToDPWDateFormat(shipment.getPickupDetails().getActualPickupOrDelivery()));
        }
        processPackingCommodityGroup(pack, dict);
        if(pack.getPacks() != null) {
            dict.put(PACKS, getDPWWeightVolumeFormat(new BigDecimal(pack.getPacks()), 0, v1TenantSettingsResponse) );
        }
        if (pack.getPacksType() != null)
        {
            dict.put(ReportConstants.SHIPMENT_PACKING_PACKS_PACKSTYPE, pack.getPacksType());
        }

        dict.put(SHIPMENT_PACKING_LENGTH, getDPWWeightVolumeFormat(pack.getLength(), 0, v1TenantSettingsResponse));
        dict.put(SHIPMENT_PACKING_LENGTH_UNIT, pack.getLengthUnit());
        dict.put(SHIPMENT_PACKING_WIDTH, getDPWWeightVolumeFormat(pack.getWidth(), 0, v1TenantSettingsResponse));
        dict.put(SHIPMENT_PACKING_WIDTH_UNIT, pack.getWidthUnit());
        dict.put(SHIPMENT_PACKING_HEIGHT, getDPWWeightVolumeFormat(pack.getHeight(), 0, v1TenantSettingsResponse));
        dict.put(SHIPMENT_PACKING_HEIGHT_UNIT, pack.getHeightUnit());
        dict.put(CHARGEABLE, convertToWeightNumberFormat(pack.getChargeable(), v1TenantSettingsResponse));
        dict.put(CHARGEABLE_UNIT1, pack.getChargeableUnit());
        dict.put(HS_CODE, pack.getHSCode());
        dict.put(DESCRIPTION, pack.getGoodsDescription());
        dict.put(IS_DG, false);
        dict.put(PACKS_MARKS_NUMBERS, pack.getMarksnNums());
        dict.put(PACKS_GOODS_DESCRIPTION, pack.getGoodsDescription());
        dict.put(PACKS_CONTAINER_NUMBER, pack.getContainerNumber());
        if(Objects.isNull(pack.getGoodsDescription())) {
            dict.put(PACKS_GOODS_DESCRIPTION, "");
            dict.put(DESCRIPTION, "");
        }
        processPackingHazardous(pack, dict);
        processPackingTempControlled(pack, dict);
        try {
            processPackingMasterData(pack);
            dict.put(SHIPMENT_PACKING_PACKS_TYPE_DESCRIPTION, getMasterListItemDesc(pack.getPacksType(), MasterDataType.PACKS_UNIT.name(), false));
        }
        catch (Exception ignored) {
            // Left blank for sonar to flag it
        }

        return dict;
    }


    public void groupPacksDetails(List<PackingModel> packingList,
                                  V1TenantSettingsResponse v1TenantSettingsResponse,
                                  Map<String, Object> dictionary,
                                  List<ContainerModel> containerList) {

        boolean hasPacks = packingList != null && !packingList.isEmpty();
        boolean hasContainers = containerList != null && !containerList.isEmpty();

        if (hasPacks) {
            // Priority: If packs exist, show packs info grouped by container
            processPacksData(packingList, v1TenantSettingsResponse, dictionary);
            dictionary.put(HAS_PACKS2, true);
            dictionary.put(HAS_CONTAINERS2, false);
        } else if (hasContainers) {
            // Fallback: If only containers exist, show container info
            processContainersData(containerList, v1TenantSettingsResponse, dictionary);
            dictionary.put(HAS_PACKS2, false);
            dictionary.put(HAS_CONTAINERS2, true);
        } else {
            // Neither packs nor containers exist
            dictionary.put(HAS_PACKS2, false);
            dictionary.put(HAS_CONTAINERS2, false);
        }
    }

    private void processPacksData(List<PackingModel> packingList,
                                  V1TenantSettingsResponse v1TenantSettingsResponse,
                                  Map<String, Object> dictionary) {

        // Group packs by container (using container ID for better accuracy)
        Map<String, List<PackingModel>> packsGroupedByContainer = packingList.stream()
                .collect(Collectors.groupingBy(pack -> {
                    if (pack.getContainerNumber() != null) {
                        return pack.getContainerNumber();
                    } else {
                        return "UNASSIGNED";
                    }
                }));

        List<Map<String, Object>> shipmentPacksList = new ArrayList<>();

        for (Map.Entry<String, List<PackingModel>> entry : packsGroupedByContainer.entrySet()) {
            List<PackingModel> packsInGroup = entry.getValue();

            Map<String, Object> groupedPackData = buildPacksGroupSummary(
                    dictionary, packsInGroup, v1TenantSettingsResponse);
            shipmentPacksList.add(groupedPackData);
        }

        // Set the data for template consumption
        dictionary.put("ShipmentPacks2", shipmentPacksList);
    }


    private void processContainersData(List<ContainerModel> containerList,
                                       V1TenantSettingsResponse settings,
                                       Map<String, Object> dictionary) {

        List<Map<String, Object>> shipmentContainersList = new ArrayList<>();

        for (ContainerModel container : containerList) {
            Map<String, Object> containerData = new HashMap<>();

            String grossWeight = formatWeight(container.getGrossWeight(), settings);
            String grossVolume = formatVolume(container.getGrossVolume(), settings);

            // Put values into containerData and dictionary simultaneously
            putValue(containerData, dictionary, CONTAINER_TYPE_CODE2, container.getContainerCode());
            putValue(containerData, dictionary, CONTAINER_NUMBER2, container.getContainerNumber());
            putValue(containerData, dictionary, CARRIER_SEAL_NUMBER2, container.getCarrierSealNumber());
            putValue(containerData, dictionary, MARKS_NUMS2, container.getMarksNums());
            putValue(containerData, dictionary, GOODS_DESCRIPTION2, container.getDescriptionOfGoods());

            String packs = container.getPacks();
            putValue(containerData, dictionary, PACKS2, (packs == null || packs.isEmpty()) ? "" : packs);
            putValue(containerData, dictionary, PACKS_UNIT2, container.getPacksType());

            putValue(containerData, dictionary, GROSS_WEIGHT2, grossWeight);

            putValue(containerData, dictionary, GROSS_WEIGHT_UNIT2, isEmpty(grossWeight) ? "" : container.getGrossWeightUnit());

            putValue(containerData, dictionary, GROSS_VOLUME2, grossVolume);

            putValue(containerData, dictionary, GROSS_VOLUME_UNIT2, isEmpty(grossVolume) ? "" : container.getGrossVolumeUnit());

            shipmentContainersList.add(containerData);
        }

        dictionary.put("ShipmentContainers2", shipmentContainersList);
    }

    private void putValue(Map<String, Object> map1, Map<String, Object> map2, String key, String value) {
        String safeValue = (value == null) ? "" : value;
        map1.put(key, safeValue);
        map2.put(key, safeValue);
    }

    private boolean isEmpty(String value) {
        return value == null || value.trim().isEmpty();
    }


    private Map<String, Object> buildPacksGroupSummary(Map<String, Object> dictionary,
                                                       List<PackingModel> packsInGroup,
                                                       V1TenantSettingsResponse settings) {
        Set<String> marksSet = new LinkedHashSet<>();
        Set<String> descSet = new LinkedHashSet<>();
        Set<String> packUnits = new LinkedHashSet<>();

        BigDecimal totalPacks = BigDecimal.ZERO;
        BigDecimal totalWeight = BigDecimal.ZERO;
        BigDecimal totalVolume = BigDecimal.ZERO;

        Containers containerData = null;
        String containerNumber = null;

        // Aggregate pack data and first container data
        for (PackingModel pack : packsInGroup) {
            if (containerData == null && pack.getContainerId() != null) {
                containerData = containerDao.findById(pack.getContainerId()).orElse(null);
            }

            addIfNotBlank(marksSet, pack.getMarksnNums());
            addIfNotBlank(descSet, pack.getGoodsDescription());
            addIfNotNull(packUnits, pack.getPacksType());

            totalPacks = totalPacks.add(safeBigDecimal(pack.getPacks()));
            totalWeight = totalWeight.add(convertToKg(pack.getWeight(), pack.getWeightUnit()));
            totalVolume = totalVolume.add(convertToM3(pack.getVolume(), pack.getVolumeUnit()));

            if (containerNumber == null && isNotBlank(pack.getContainerNumber())) {
                containerNumber = pack.getContainerNumber();
            }
        }

        containerNumber = firstNonNull(containerNumber, containerData != null ? containerData.getContainerNumber() : null);
        String containerTypeCode = containerData != null ? containerData.getContainerCode() : "";
        String carrierSealNumber = containerData != null ? containerData.getCarrierSealNumber() : "";

        Map<String, Object> summary = new HashMap<>();

        putBoth(summary, dictionary, CONTAINER_TYPE_CODE2, containerTypeCode);
        putBoth(summary, dictionary, CONTAINER_NUMBER2, containerNumber != null ? containerNumber : "");
        putBoth(summary, dictionary, CARRIER_SEAL_NUMBER2, carrierSealNumber);

        putBoth(summary, dictionary, PACKS_MARKS_NUMBER2, String.join(", ", marksSet));
        putBoth(summary, dictionary, PACKS_GOODS_DESCRIPTION2, String.join("\n", descSet));
        putBoth(summary, dictionary, PACKS2, formatIfPositive(totalPacks, 0, settings));

        String packUnit = determinePackUnit(packUnits);
        putBoth(summary, dictionary, PACKS_UNIT2, packUnit);

        final String weightUnit = "KG";
        final String volumeUnit = "M3";

        putBoth(summary, dictionary, GROSS_WEIGHT2, formatIfPositive(totalWeight, 2, settings));
        putBoth(summary, dictionary, GROSS_WEIGHT_UNIT2, totalWeight.compareTo(BigDecimal.ZERO) > 0 ? weightUnit : "");

        putBoth(summary, dictionary, GROSS_VOLUME2, formatIfPositive(totalVolume, 2, settings));
        putBoth(summary, dictionary, GROSS_VOLUME_UNIT2, totalVolume.compareTo(BigDecimal.ZERO) > 0 ? volumeUnit : "");

        return summary;
    }

    // Helper to add non-null, non-blank trimmed strings to set
    private void addIfNotBlank(Set<String> set, String value) {
        if (value != null && !value.trim().isEmpty()) {
            set.add(value.trim());
        }
    }

    // Helper to add non-null values to set
    private <T> void addIfNotNull(Set<T> set, T value) {
        if (value != null) {
            set.add(value);
        }
    }

    // Helper to safely convert to BigDecimal or zero if null or invalid
    private BigDecimal convertToKg(Object weightValue, String unit) {
        BigDecimal weight = safeBigDecimal(weightValue);
        if (weight.equals(BigDecimal.ZERO)) return BigDecimal.ZERO;

        if (unit == null) return weight; // Assume KG if no unit

        return switch (unit.toUpperCase()) {
            case "KG" -> weight;
            case "G" -> weight.divide(new BigDecimal("1000"), 6, RoundingMode.HALF_UP);
            case "MG", "MC" -> weight.divide(new BigDecimal(TEN_LAKH), 6, RoundingMode.HALF_UP);
            case "LB" -> weight.multiply(new BigDecimal("0.453592"));
            case "OT" ->
                    weight.multiply(new BigDecimal("28.3495")).divide(new BigDecimal("1000"), 6, RoundingMode.HALF_UP);
            case "MT" -> weight.multiply(new BigDecimal("1000"));
            case "KT" -> weight.multiply(new BigDecimal(TEN_LAKH));
            case "LT" -> weight.multiply(new BigDecimal("1016.05"));
            case "HG", "DT" -> weight.divide(new BigDecimal("10"), 6, RoundingMode.HALF_UP);
            default -> weight; // Assume KG for unknown units
        };
    }

    private BigDecimal safeBigDecimal(Object value) {
        if (value == null) return BigDecimal.ZERO;
        if (value instanceof BigDecimal) return (BigDecimal) value;
        try {
            return new BigDecimal(value.toString());
        } catch (Exception e) {
            return BigDecimal.ZERO;
        }
    }

    private BigDecimal convertToM3(Object volumeValue, String unit) {
        BigDecimal volume = safeBigDecimal(volumeValue);
        if (volume.equals(BigDecimal.ZERO)) return BigDecimal.ZERO;

        if (unit == null) return volume; // Assume M3 if no unit

        return switch (unit.toUpperCase()) {
            case "M3" -> volume;
            case "L" -> volume.divide(new BigDecimal("1000"), 6, RoundingMode.HALF_UP);
            case "ML" -> volume.divide(new BigDecimal(TEN_LAKH), 6, RoundingMode.HALF_UP);
            case "CC" -> volume.divide(new BigDecimal(TEN_LAKH), 6, RoundingMode.HALF_UP); // Same as ML
            case "CF" -> volume.multiply(new BigDecimal("0.0283168")); // Cubic feet
            case "CI" -> volume.multiply(new BigDecimal("0.0000163871")); // Cubic inches
            case "CY" -> volume.multiply(new BigDecimal("0.764555")); // Cubic yards
            case "D3" -> volume.divide(new BigDecimal("1000"), 6, RoundingMode.HALF_UP); // Cubic decimeters (same as liters)
            case "GA" -> volume.multiply(new BigDecimal("0.00378541")); // US Gallons
            case "GI" -> volume.multiply(new BigDecimal("0.00454609")); // Imperial Gallons
            default -> volume; // Assume M3 for unknown units
        };
    }

    // Returns the first non-null value, else null
    private <T> T firstNonNull(T first, T second) {
        return first != null ? first : second;
    }

    // Helper to put key-value in both maps
    private void putBoth(Map<String, Object> map1, Map<String, Object> map2, String key, Object value) {
        map1.put(key, value);
        map2.put(key, value);
    }

    // Helper to format BigDecimal if positive, else empty string
    private String formatIfPositive(BigDecimal value, int scale, V1TenantSettingsResponse settings) {
        if (value.compareTo(BigDecimal.ZERO) > 0) {
            return getDPWWeightVolumeFormat(value, scale, settings);
        }
        return "";
    }

    // Determine pack unit string based on how many units are in set
    private String determinePackUnit(Set<String> packUnits) {
        if (packUnits == null || packUnits.isEmpty()) {
            return PACKAGES;
        }
        if (packUnits.size() == 1) {
            return packUnits.iterator().next();
        }
        return PACKAGES;
    }

    // Helper to check non-blank string
    private boolean isNotBlank(String str) {
        return str != null && !str.trim().isEmpty();
    }

    // Helper methods for formatting
    private String formatWeight(BigDecimal weight, V1TenantSettingsResponse settings) {
        if (weight == null || weight.compareTo(BigDecimal.ZERO) == 0) {
            return "";
        }
        return getDPWWeightVolumeFormat(weight, 2, settings);
    }

    private String formatVolume(BigDecimal volume, V1TenantSettingsResponse settings) {
        if (volume == null || volume.compareTo(BigDecimal.ZERO) == 0) {
            return "";
        }
        return getDPWWeightVolumeFormat(volume, 2, settings);
    }

    // Updated method signature to include container list
    public void getContainerDetails(ShipmentModel shipmentModel, Map<String, Object> dictionary) {
        List<ContainerModel> containerModelList = shipmentModel.getContainersList();
        List<PackingModel> packingList = shipmentModel.getPackingList();

        // Use the main grouping method with proper business logic
        groupPacksDetails(packingList, getCurrentTenantSettings(), dictionary, containerModelList);
    }

    private void processStringUtilityTags(PackingModel pack, Map<String, Object> dict) {
        if(!StringUtility.isEmpty(pack.getUnNumber()))
            dict.put(OCEAN_UN_NUMBER, pack.getUnNumber());
        if(!StringUtility.isEmpty(pack.getProperShippingName()))
            dict.put(OCEAN_DG_PSN, pack.getProperShippingName());
        processPackingMasterData(pack);
        if(!StringUtility.isEmpty(pack.getDGClass()))
            dict.put(OCEAN_DG_CLASS, getMasterListItemDesc(pack.getDGClass(), MasterDataType.DG_CLASS.name(), true));
        processPackingMinimumFlashPoint(pack, dict);
        if(!StringUtility.isEmpty(pack.getPackingGroup()))
            dict.put(PACKING_GROUP, getMasterListItemDesc(pack.getPackingGroup(), MasterDataType.PACKING_GROUP.name(), true));
    }

    private void processPackingTempControlled(PackingModel pack, Map<String, Object> dict) {
        if(pack.getIsTemperatureControlled() != null && pack.getIsTemperatureControlled().equals(true)) {
            dict.put(MIN_TEMP, pack.getMinTemp());
            dict.put(MAX_TEMP, pack.getMaxTemp());
            dict.put(MIN_TEMP_UNIT, pack.getMinTempUnit());
            dict.put(MAX_TEMP_UNIT, pack.getMaxTempUnit());
        } else {
            dict.put(MIN_TEMP, "");
            dict.put(MAX_TEMP, "");
            dict.put(MIN_TEMP_UNIT, "");
            dict.put(MAX_TEMP_UNIT, "");
        }
    }

    private void processPackingHazardous(PackingModel pack, Map<String, Object> dict) {
        if(pack.getHazardous() != null && pack.getHazardous().equals(true)){
            var dgSubstanceRow = masterDataUtils.fetchDgSubstanceRow(pack.getDGSubstanceId());
            dict.put(DG_SUBSTANCE, dgSubstanceRow.ProperShippingName);
            dict.put(DG_CLASS, pack.getDGClass());
            dict.put(CLASS_DIVISION, dgSubstanceRow.ClassDivision);
            dict.put(UNID_NO, pack.getUNDGContact());
            dict.put(DANGEROUS_GOODS, "HAZARDOUS");
            dict.put(IS_DG, true);
            dict.put(AIR_UN_NUMBER, pack.getUnNumberAir());
            dict.put(AIR_DG_CLASS, pack.getDgClassAir());
            dict.put(AIR_DG_CLASS_DESCRIPTION, pack.getDgClassAirDescription());
        } else {
            dict.put(DG_SUBSTANCE, "");
            dict.put(DG_CLASS, "");
            dict.put(CLASS_DIVISION, "");
            dict.put(UNID_NO, "");
            dict.put(DANGEROUS_GOODS, "General");
        }
    }

    private void processPackingCommodityGroup(PackingModel pack, Map<String, Object> dict) {
        if(pack.getCommodityGroup() != null) {
            MasterData commodity = getMasterListData(MasterDataType.COMMODITY_GROUP, pack.getCommodityGroup());
            if(!Objects.isNull(commodity))
                dict.put(PACKS_COMMODITY_GROUP, commodity.getItemDescription());
        }
    }

    private void processPackingMinimumFlashPoint(PackingModel pack, Map<String, Object> dict) {
        if(pack.getMinimumFlashPoint() != null)
        {
            String flashPointAndUnit = String.valueOf(pack.getMinimumFlashPoint());
            if(!StringUtility.isEmpty(pack.getMinimumFlashPointUnit()))
                flashPointAndUnit = flashPointAndUnit + " " + pack.getMinimumFlashPointUnit();
            dict.put(FLASH_POINT_AND_UNIT, flashPointAndUnit);
        }
    }

    private void processPackCommodity(PackingModel pack, Map<String, Object> dict, Map<String, EntityTransferCommodityType> commodityTypeMap) {
        if(pack.getCommodity() != null) {
            dict.put(COMMODITY_DESC, pack.getCommodity());
            if(commodityTypeMap != null && commodityTypeMap.containsKey(pack.getCommodity()))
                dict.put(COMMODITY_DESC_NAME, commodityTypeMap.get(pack.getCommodity()).getDescription());
        }
    }

    private Map<String, EntityTransferCommodityType> getCommodityTypeMap(ShipmentModel shipment) {
        Map<String, EntityTransferCommodityType> commodityTypeMap = new HashMap<>();
        try{
            List<String> commodityCodes = shipment.getPackingList().stream().map(PackingModel::getCommodity).toList();
            if(!commodityCodes.isEmpty())
                commodityTypeMap = masterDataUtils.fetchInBulkCommodityTypes(commodityCodes);
        } catch (Exception e) {
            log.error("Error while ");
        }
        return commodityTypeMap;
    }

    private void processPackingWeightVolume(PackingModel pack, Map<String, Object> dict, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(pack.getWeight() != null){
            dict.put(WEIGHT_AND_UNIT_PACKS, String.format(REGEX_S_S, convertToWeightNumberFormat(pack.getWeight(), v1TenantSettingsResponse),
                    pack.getWeightUnit()));
        }
        if(pack.getVolume() != null){
            dict.put(VOLUME_AND_UNIT_PACKS, String.format(REGEX_S_S, convertToVolumeNumberFormat(pack.getVolume(), v1TenantSettingsResponse),
                    pack.getVolumeUnit()));
        }
        if (pack.getVolumeWeight() != null) {
            dict.put(V_WEIGHT_AND_UNIT_PACKS, String.format(REGEX_S_S, convertToWeightNumberFormat(pack.getVolumeWeight(), v1TenantSettingsResponse),
                    pack.getVolumeWeightUnit()));
        }
    }

    public void processPackingMasterData(PackingModel pack) {
        try {
            Set<MasterListRequest> requests = new HashSet<>();
            Optional<Cache> cacheOptional = Optional.ofNullable(cacheManager.getCache(CacheConstants.CACHE_KEY_MASTER_DATA));
            Cache.ValueWrapper value1 = cacheOptional.map(c -> c.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, pack.getDGClass()))).orElse(null);
            if(Objects.isNull(value1))
                requests.add(MasterListRequest.builder().ItemType(MasterDataType.DG_CLASS.getDescription()).ItemValue(pack.getDGClass()).Cascade(null).build());
            Cache.ValueWrapper value2 = cacheOptional.map(c -> c.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, pack.getPackingGroup()))).orElse(null);
            if(Objects.isNull(value2))
                requests.add(MasterListRequest.builder().ItemType(MasterDataType.PACKING_GROUP.getDescription()).ItemValue(pack.getPackingGroup()).Cascade(null).build());
            Cache.ValueWrapper value3 = cacheOptional.map(c -> c.get(keyGenerator.customCacheKeyForMasterData(CacheConstants.MASTER_LIST, pack.getPacksType()))).orElse(null);
            if(Objects.isNull(value3))
                requests.add(MasterListRequest.builder().ItemType(MasterDataType.PACKS_UNIT.getDescription()).ItemValue(pack.getPacksType()).Cascade(null).build());

            if(!requests.isEmpty()) {
                MasterListRequestV2 masterListRequestV2 = new MasterListRequestV2();
                masterListRequestV2.setMasterListRequests(requests.stream().toList());
                masterListRequestV2.setIncludeCols(Arrays.asList("ItemType", MasterDataConstants.ITEM_VALUE, "ItemDescription", "ValuenDesc", "Cascade"));
                Map<String, EntityTransferMasterLists> keyMasterDataMap = masterDataUtils.fetchInBulkMasterList(masterListRequestV2);
                Set<String> keys = new HashSet<>();
                commonUtils.createMasterDataKeysList(requests, keys);
                masterDataUtils.pushToCache(keyMasterDataMap, CacheConstants.MASTER_LIST, keys, new EntityTransferMasterLists(), null);
            }
        } catch (Exception ignored) { log.info(Constants.IGNORED_ERROR_MSG); }
    }

    public void populateBillChargesFields(ShipmentModel shipment, Map<String, Object> dictionary) {
        List<BillingResponse> billingsList = null;
        try {
            billingsList = getBillingData(shipment.getGuid());
        }
        catch (Exception e) {
            log.error(e.getMessage());
        }
        List<BillChargesResponse> charges = getBillChargesResponses(dictionary, billingsList);

        List<BillChargesResponse> originalChargesRows = new ArrayList<>();
        List<BillChargesResponse> copyChargesRows = new ArrayList<>();
        dictionary.put(AS_AGREED, false);
        dictionary.put(COPY_AS_AGREED, false);
        var v1TenantSettingsResponse = getCurrentTenantSettings();
        String chargesApply = shipment.getAdditionalDetails() != null ? shipment.getAdditionalDetails().getBLChargesDisplay() : null;

        if (!Objects.isNull(chargesApply) && chargesApply.equals("AGR")) {
            dictionary.put(AS_AGREED, true);
            dictionary.put(COPY_AS_AGREED, true);
        } else if (!Objects.isNull(chargesApply) && (chargesApply.equals("CPP") || chargesApply.equals("CAL") || chargesApply.equals("CCL"))) {
            dictionary.put(AS_AGREED, true);
        }

        if (Objects.isNull(chargesApply) || chargesApply.equals("NON")) {
            dictionary.put(HAS_CHARGES, false);
        } else {
            dictionary.put(HAS_CHARGES, true);
            getChargeRows(originalChargesRows, copyChargesRows, charges, chargesApply);
        }
        List<BillChargesResponse> originalChargesRowsInCaps = convertChargeRowsToUpperCase(originalChargesRows);
        dictionary.put(CHARGES_IN_CAPS, originalChargesRowsInCaps);
        dictionary.put(CHARGES_SMALL, originalChargesRows);

        if(!originalChargesRows.isEmpty() && !originalChargesRowsInCaps.isEmpty())
            processOriginalChargesRows(dictionary, originalChargesRows, originalChargesRowsInCaps, v1TenantSettingsResponse);
        dictionary.put(COPY_CHARGES, copyChargesRows);
    }

    private List<BillChargesResponse> getBillChargesResponses(Map<String, Object> dictionary, List<BillingResponse> billingsList) {
        List<BillChargesResponse> charges = new ArrayList<>();
        BillingResponse billRow = null;
        if(billingsList != null && !billingsList.isEmpty()) {
            billRow = billingsList.get(0);
            for(BillingResponse billingResponse : billingsList) {
                List<BillChargesResponse> billChargesResponses = getBillChargesData(billingResponse);
                if(billChargesResponses != null) {
                    charges.addAll(billChargesResponses);
                }
            }
        }
        dictionary.put(ReportConstants.BILL_REMARKS, billRow != null ? billRow.getRemarks() : "");
        return charges;
    }

    private void processOriginalChargesRows(Map<String, Object> dictionary, List<BillChargesResponse> originalChargesRows, List<BillChargesResponse> originalChargesRowsInCaps, V1TenantSettingsResponse v1TenantSettingsResponse) {
        List<Map<String, Object>> values = new ArrayList<>();
        List<Map<String, Object>> valuesInCaps = new ArrayList<>();
        for (BillChargesResponse billChargesResponse : originalChargesRows) {
            values.add(jsonHelper.convertValue(billChargesResponse, new TypeReference<>() {}));
        }
        for (BillChargesResponse billChargesResponseInCaps : originalChargesRowsInCaps) {
            valuesInCaps.add(jsonHelper.convertValue(billChargesResponseInCaps, new TypeReference<>() {}));
        }
        for (Map<String, Object> v: values) {
            if(v.containsKey(OVERSEAS_SELL_AMOUNT) && v.get(OVERSEAS_SELL_AMOUNT) != null) {
                v.put(OVERSEAS_SELL_AMOUNT, AmountNumberFormatter.format(new BigDecimal(StringUtility.convertToString(v.get(OVERSEAS_SELL_AMOUNT))), StringUtility.convertToString(v.get("OverseasSellCurrency")), v1TenantSettingsResponse));
            }
        }
        for (Map<String, Object> v: valuesInCaps) {
            if(v.containsKey(OVERSEAS_SELL_AMOUNT) && v.get(OVERSEAS_SELL_AMOUNT) != null) {
                v.put(OVERSEAS_SELL_AMOUNT, StringUtility.toUpperCase(AmountNumberFormatter.format(new BigDecimal(StringUtility.convertToString(v.get(OVERSEAS_SELL_AMOUNT))), StringUtility.convertToString(v.get("OverseasSellCurrency")), v1TenantSettingsResponse)));
            }
        }
        dictionary.put(CHARGES_SMALL, values);
        dictionary.put(CHARGES_IN_CAPS, valuesInCaps);
    }

    private List<BillChargesResponse> convertChargeRowsToUpperCase(List<BillChargesResponse> originalChargesRows) {
        List<BillChargesResponse> originalChargesRowsInCaps = new ArrayList<>();
        if (originalChargesRows == null) {
            return originalChargesRowsInCaps;
        }
        for (BillChargesResponse originalChargeRow : originalChargesRows) {
            convertFieldsToUpperCase(originalChargeRow);
            originalChargesRowsInCaps.add(originalChargeRow);
        }
        return originalChargesRowsInCaps;
    }

    private void convertFieldsToUpperCase(BillChargesResponse chargeRow) {
        chargeRow.setOverseasSellCurrency(toUpperCase(chargeRow.getOverseasSellCurrency()));
        chargeRow.setLocalSellCurrency(toUpperCase(chargeRow.getLocalSellCurrency()));
        chargeRow.setPaymentType(toUpperCase(chargeRow.getPaymentType()));
        chargeRow.setChargeTypeCode(toUpperCase(chargeRow.getChargeTypeCode()));
        chargeRow.setChargeTypeDescription(toUpperCase(chargeRow.getChargeTypeDescription()));
        chargeRow.setMeasurementBasis(toUpperCase(chargeRow.getMeasurementBasis()));
        chargeRow.setLocalCostCurrency(toUpperCase(chargeRow.getLocalCostCurrency()));
        chargeRow.setBillingChargeTypeId(toUpperCase(chargeRow.getBillingChargeTypeId()));
        chargeRow.setBillingChargeTypeGuid(toUpperCase(chargeRow.getBillingChargeTypeGuid()));
    }

    private String toUpperCase(String value) {
        return value != null ? value.toUpperCase() : null;
    }
    private void getChargeRows(List<BillChargesResponse> originalChargesRows, List<BillChargesResponse> copyChargesRows, List<BillChargesResponse> charges, String type) {
        List<BillChargesResponse> prepaid = charges.stream().filter(x -> !Strings.isNullOrEmpty(x.getPaymentType()) && x.getPaymentType().equals("PPD")).toList();
        List<BillChargesResponse> collect = charges.stream().filter(x -> !Strings.isNullOrEmpty(x.getPaymentType()) && x.getPaymentType().equals("CCX")).toList();

        switch (type)
        {
            case "CPP":
                copyChargesRows.addAll(prepaid);
                break;
            case "CAL":
                copyChargesRows.addAll(charges);
                break;

            case "PPD":
                originalChargesRows.addAll(prepaid);
                copyChargesRows.addAll(prepaid);
                break;

            case "SHW":
                originalChargesRows.addAll(collect);
                copyChargesRows.addAll(collect);
                break;
            case "ALL":
                originalChargesRows.addAll(charges);
                copyChargesRows.addAll(charges);
                break;

            case "CCL":
                copyChargesRows.addAll(collect);
                break;
            default:
                break;

        }
    }
    public void populateShipmentOrganizationsLL(ShipmentModel shipmentDetails, Map<String, Object> dictionary, List<String> orgWithoutTranslation) {
        var languageCode = UserContext.getUser().getLanguageCode();
        List<AddressTranslationRequest.OrgAddressCode> orgAddressCodeList = getOrgAddressCodesList(shipmentDetails);

        if(Strings.isNullOrEmpty(languageCode) || orgAddressCodeList.isEmpty()){
            return;
        }
        AddressTranslationRequest request = AddressTranslationRequest.builder()
                .OrgAddressCodeList(orgAddressCodeList)
                .LanguageCode(languageCode)
                .build();
        AddressTranslationListResponse response = null;
        try {
            response = v1Service.getAddressTranslation(request);
        } catch (Exception ex) {
            log.error(ex.getMessage());
        }
        Map<String, AddressTranslationListResponse.AddressTranslationResponse> orgVsAddressTranslationMap = new HashMap<>();
        if(!Objects.isNull(response) && !Objects.isNull(response.getAddressTranslationList()) && !response.getAddressTranslationList().isEmpty()){
            orgVsAddressTranslationMap =
                    response.getAddressTranslationList().stream().collect(Collectors.toMap(obj -> obj.getOrgCode() + "_" + obj.getAddressCode(), obj -> obj));
        }
        processShipmentClient(shipmentDetails, dictionary, orgWithoutTranslation, orgVsAddressTranslationMap);
        processShipmentConsigner(shipmentDetails, dictionary, orgWithoutTranslation, orgVsAddressTranslationMap);
        processShipmentConsignee(shipmentDetails, dictionary, orgWithoutTranslation, orgVsAddressTranslationMap);
        processShipmentNotifyParty(shipmentDetails, dictionary, orgWithoutTranslation, orgVsAddressTranslationMap);
        processShipemtnPickupDetails(shipmentDetails, dictionary, orgWithoutTranslation, orgVsAddressTranslationMap);
        processShipmentDeliveryDetails(shipmentDetails, dictionary, orgWithoutTranslation, orgVsAddressTranslationMap);
    }

    private void processShipmentClient(ShipmentModel shipmentDetails, Map<String, Object> dictionary, List<String> orgWithoutTranslation, Map<String, AddressTranslationListResponse.AddressTranslationResponse> orgVsAddressTranslationMap) {
        if(!Objects.isNull(shipmentDetails.getClient()) && !Strings.isNullOrEmpty(shipmentDetails.getClient().getOrgCode())){
            String orgCode = shipmentDetails.getClient().getOrgCode();
            String addressCode = shipmentDetails.getClient().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(CLIENT_LL, address.getOrgName());
                dictionary.put(CLIENT_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
                dictionary.put(CLIENT_ADDWO_CONT_LL, ReportHelper.getOrgAddressWithoutPhoneEmail(address.getOrgName(), address.getAddress(), null, address.getCityName(), address.getStateName(), address.getPostalCode(), null ));
            } else {
                orgWithoutTranslation.add("Client");
            }
        }
    }

    private void processShipmentConsigner(ShipmentModel shipmentDetails, Map<String, Object> dictionary, List<String> orgWithoutTranslation, Map<String, AddressTranslationListResponse.AddressTranslationResponse> orgVsAddressTranslationMap) {
        if(!Objects.isNull(shipmentDetails.getConsigner()) && !Strings.isNullOrEmpty(shipmentDetails.getConsigner().getOrgCode())){
            String orgCode = shipmentDetails.getConsigner().getOrgCode();
            String addressCode = shipmentDetails.getConsigner().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(CONSIGNER_LL, address.getOrgName());
                dictionary.put(CONSIGNER_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
                dictionary.put(CONSIGNER_ADDWO_CONT_LL, ReportHelper.getOrgAddressWithoutPhoneEmail(address.getOrgName(), address.getAddress(), null, address.getCityName(), address.getStateName(), address.getPostalCode(), null ));
            } else {
                orgWithoutTranslation.add("Consigner");
            }
        }
    }

    private void processShipmentConsignee(ShipmentModel shipmentDetails, Map<String, Object> dictionary, List<String> orgWithoutTranslation, Map<String, AddressTranslationListResponse.AddressTranslationResponse> orgVsAddressTranslationMap) {
        if(!Objects.isNull(shipmentDetails.getConsignee()) && !Strings.isNullOrEmpty(shipmentDetails.getConsignee().getOrgCode())){
            String orgCode = shipmentDetails.getConsignee().getOrgCode();
            String addressCode = shipmentDetails.getConsignee().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(CONSIGNEE_LL, address.getOrgName());
                dictionary.put(CONSIGNEE_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
                dictionary.put(CONSIGNEE_ADDWO_CONT_LL, ReportHelper.getOrgAddressWithoutPhoneEmail(address.getOrgName(), address.getAddress(), null, address.getCityName(), address.getStateName(), address.getPostalCode(), null ));
            } else {
                orgWithoutTranslation.add("Consignee");
            }
        }
    }

    private void processShipmentNotifyParty(ShipmentModel shipmentDetails, Map<String, Object> dictionary, List<String> orgWithoutTranslation, Map<String, AddressTranslationListResponse.AddressTranslationResponse> orgVsAddressTranslationMap) {
        if(!Objects.isNull(shipmentDetails.getAdditionalDetails()) && !Objects.isNull(shipmentDetails.getAdditionalDetails().getNotifyParty()) && !Strings.isNullOrEmpty(shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgCode())){
            String orgCode = shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgCode();
            String addressCode = shipmentDetails.getAdditionalDetails().getNotifyParty().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(NOTIFY_PARTY_LL, address.getOrgName());
                dictionary.put(NOTIFY_PARTY_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
                dictionary.put(NOTIFY_PART_ADDWO_CONT_LL, ReportHelper.getOrgAddressWithoutPhoneEmail(address.getOrgName(), address.getAddress(), null, address.getCityName(), address.getStateName(), address.getPostalCode(), null ));
            } else {
                orgWithoutTranslation.add("Notify Party");
            }
        }
    }

    private void processShipemtnPickupDetails(ShipmentModel shipmentDetails, Map<String, Object> dictionary, List<String> orgWithoutTranslation, Map<String, AddressTranslationListResponse.AddressTranslationResponse> orgVsAddressTranslationMap) {
        if(!Objects.isNull(shipmentDetails.getPickupDetails()) && !Objects.isNull(shipmentDetails.getPickupDetails().getSourceDetail()) && !Strings.isNullOrEmpty(shipmentDetails.getPickupDetails().getSourceDetail().getOrgCode())){
            String orgCode = shipmentDetails.getPickupDetails().getSourceDetail().getOrgCode();
            String addressCode = shipmentDetails.getPickupDetails().getSourceDetail().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(PICKUP_FROM_LL, address.getOrgName());
                dictionary.put(PICKUP_FROM_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
            } else {
                orgWithoutTranslation.add("PickupFrom");
            }
        }
    }

    private void processShipmentDeliveryDetails(ShipmentModel shipmentDetails, Map<String, Object> dictionary, List<String> orgWithoutTranslation, Map<String, AddressTranslationListResponse.AddressTranslationResponse> orgVsAddressTranslationMap) {
        if(!Objects.isNull(shipmentDetails.getDeliveryDetails()) && !Objects.isNull(shipmentDetails.getDeliveryDetails().getDestinationDetail()) && !Strings.isNullOrEmpty(shipmentDetails.getDeliveryDetails().getDestinationDetail().getOrgCode())){
            String orgCode = shipmentDetails.getDeliveryDetails().getDestinationDetail().getOrgCode();
            String addressCode = shipmentDetails.getDeliveryDetails().getDestinationDetail().getAddressCode();
            if(orgVsAddressTranslationMap.containsKey(orgCode + "_" + addressCode)){
                AddressTranslationListResponse.AddressTranslationResponse address = orgVsAddressTranslationMap.get(orgCode + "_" + addressCode);
                dictionary.put(DELIVERY_TO_LL, address.getOrgName());
                dictionary.put(DELIVERY_TO_ADDRESS_LL, ReportHelper.getOrgAddress(null, address.getAddress(), null, null, ReportHelper.combineStringsWithComma(address.getCityName(),address.getPostalCode()), address.getStateName()));
            } else {
                orgWithoutTranslation.add("DeliveryTo");
            }
        }
    }

    private List<AddressTranslationRequest.OrgAddressCode> getOrgAddressCodesList(ShipmentModel shipmentDetails) {
        List<AddressTranslationRequest.OrgAddressCode> orgAddressCodeList = new ArrayList<>();
        if(!Objects.isNull(shipmentDetails.getClient()) && !Strings.isNullOrEmpty(shipmentDetails.getClient().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getClient().getOrgCode(), shipmentDetails.getClient().getAddressCode()));
        if(!Objects.isNull(shipmentDetails.getConsigner()) && !Strings.isNullOrEmpty(shipmentDetails.getConsigner().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getConsigner().getOrgCode(), shipmentDetails.getConsigner().getAddressCode()));
        if(!Objects.isNull(shipmentDetails.getConsignee()) && !Strings.isNullOrEmpty(shipmentDetails.getConsignee().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getConsignee().getOrgCode(), shipmentDetails.getConsignee().getAddressCode()));
        if(!Objects.isNull(shipmentDetails.getAdditionalDetails()) && !Objects.isNull(shipmentDetails.getAdditionalDetails().getNotifyParty()) && !Strings.isNullOrEmpty(shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getAdditionalDetails().getNotifyParty().getOrgCode(), shipmentDetails.getAdditionalDetails().getNotifyParty().getAddressCode()));
        if(!Objects.isNull(shipmentDetails.getPickupDetails()) && !Objects.isNull(shipmentDetails.getPickupDetails().getSourceDetail()) && !Strings.isNullOrEmpty(shipmentDetails.getPickupDetails().getSourceDetail().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getPickupDetails().getSourceDetail().getOrgCode(), shipmentDetails.getPickupDetails().getSourceDetail().getAddressCode()));
        if(!Objects.isNull(shipmentDetails.getDeliveryDetails()) && !Objects.isNull(shipmentDetails.getDeliveryDetails().getDestinationDetail()) && !Strings.isNullOrEmpty(shipmentDetails.getDeliveryDetails().getDestinationDetail().getOrgCode()))
            orgAddressCodeList.add(new AddressTranslationRequest.OrgAddressCode(shipmentDetails.getDeliveryDetails().getDestinationDetail().getOrgCode(), shipmentDetails.getDeliveryDetails().getDestinationDetail().getAddressCode()));
        return orgAddressCodeList;
    }

    public String getChargeTypeDescriptionLL(String chargeCode, List<String> chargeTypesWithoutTranslation) {
        var languageCode = UserContext.getUser().getLanguageCode();
        if(Strings.isNullOrEmpty(languageCode) || Strings.isNullOrEmpty(chargeCode)){
            return null;
        }
        NPMFetchMultiLangChargeCodeRequest request = NPMFetchMultiLangChargeCodeRequest.builder()
                .key(chargeCode)
                .lang(languageCode)
                .key_type("charge_code_desc")
                .build();
        try {
            NPMFetchLangChargeCodeResponse response = npmServiceAdapter.fetchMultiLangChargeCode(CommonRequestModel.buildRequest(request));
            if(Objects.isNull(response) || StringUtility.isEmpty(response.getTranslation())) {
                chargeTypesWithoutTranslation.add(chargeCode);
                return null;
            }
            return response.getTranslation();
        } catch (Exception ex) {
            throw new ValidationException("NPM service response failed for ChargeType translation due to: " + ex.getMessage());
        }
    }

    public void handleTranslationErrors(Boolean printWithoutTranslation, List<String> orgWithoutTranslation, List<String> chargeTypesWithoutTranslation) {
        if(! Boolean.TRUE.equals(printWithoutTranslation)) {
            // Throw validation exception
            StringBuilder errorMessage = new StringBuilder();
            if(! orgWithoutTranslation.isEmpty()) {
                String failedOrgs = String.join(" ,", orgWithoutTranslation);
                errorMessage.append(String.format("Translation not available for orgs : %s", failedOrgs));
                errorMessage.append("\n");
            }
            if(! chargeTypesWithoutTranslation.isEmpty()) {
                String failedChargeType = String.join(" ,", chargeTypesWithoutTranslation);
                errorMessage.append(String.format("Translation not available for Charge codes : %s", failedChargeType));
            }
            if(!errorMessage.isEmpty()) {
                throw new TranslationException(errorMessage.toString());
            }
        }
    }

    public void setContainerCount(ShipmentModel shipmentModel, Map<String, Object> dictionary) {
        var shipmentContainerList = shipmentModel.getContainersList();
        if (!shipmentContainerList.isEmpty()) {
            StringBuilder containerEtcCount = new StringBuilder(String.format("ETC %d CNTR", countAllContainers(shipmentContainerList) - 1));
            StringBuilder containerTypeValues = new StringBuilder(Constants.EMPTY_STRING);
            var containerNumbers = shipmentContainerList.stream().filter(x -> !Objects.isNull(x.getContainerNumber()))
                    .map(ContainerModel::getContainerNumber).toList();
            String containerNumber = "";
            if (!containerNumbers.isEmpty()) {
                containerNumber = containerNumbers.get(0);
            }
            dictionary.put(CONTAINER_NUMBER_WITH_ETC_COUNT, String.format(REGEX_S_S, containerNumber, containerEtcCount));

            var containerCodeMap = shipmentContainerList.stream().filter(x -> !Objects.isNull(x.getContainerCode()))
                    .collect(Collectors.groupingBy(ContainerModel::getContainerCode));

            for (String containerCode : containerCodeMap.keySet()) {
                long containerCount = containerCodeMap.get(containerCode).stream().mapToLong(ContainerModel::getContainerCount).sum();
                containerTypeValues.append(String.format("%s * %d", containerCode, containerCount));
            }
            dictionary.put(CONTAINER_COUNT_WITH_ETC_COUNT, containerTypeValues);
        }

    }

    public static String checkCreditLimitDocs(String key){
        switch (key) {
            case ReportConstants.HOUSE_BILL:
                return Constants.HBL_PRINT;
            case ReportConstants.DELIVERY_ORDER:
                return Constants.DO_PRINT;
            case ReportConstants.HAWB:
                return Constants.HAWB_PRINT;
            case ReportConstants.MAWB:
                return Constants.MAWB_PRINT;
            default:
                return null;
        }
    }
    public Long countAllContainers(List<ContainerModel> containerModels) {
        Long count = 0l;
        for(ContainerModel model : containerModels) {
            count += model.getContainerCount();
        }
        return count;
    }

    public void populateTenantFields(Map<String, Object> dictionary, TenantModel tenantModel) {
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTNAME, tenantModel.getTenantName());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTADDRESS1, tenantModel.getAddress1());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTADDRESS2, tenantModel.getAddress2());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTEMAIL, tenantModel.getEmail());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTCITY, tenantModel.getCity());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTSTATE, tenantModel.getState());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTCOUNTRY, tenantModel.getCountry());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTCONTACTPHONE, tenantModel.getPhone());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTMOBILE, tenantModel.getMobile());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTZIPPOSTCODE, tenantModel.getZipPostCode());
        dictionary.put(ReportConstants.BRANCH_BASIC_INFO_TENANTURL, tenantModel.getWebsiteUrl());
    }
    public Pair<BigDecimal, String> getTotalWeightManifest(List<ShipmentModel> shipments)
    {
        if (Objects.isNull(shipments) || shipments.isEmpty())
            return Pair.of(BigDecimal.ZERO, null);
        String weightUnit = null;
        BigDecimal totalWeight = BigDecimal.ZERO;

        for(var shipment: shipments)
        {
            if (!Strings.isNullOrEmpty(shipment.getWeightUnit()) && !Objects.isNull(shipment.getWeight()))
            {
                if (weightUnit == null)
                {
                    weightUnit = shipment.getWeightUnit();
                }

                if (!shipment.getWeightUnit().equals(weightUnit)) return Pair.of(BigDecimal.ZERO, null);
                totalWeight = totalWeight.add(shipment.getWeight());
            }
        }
        return Pair.of(totalWeight, weightUnit);
    }
    public Pair<BigDecimal, String> getTotalVolumeManifest(List<ShipmentModel> shipments)
    {
        if (Objects.isNull(shipments) || shipments.isEmpty())
            return Pair.of(BigDecimal.ZERO, null);
        String volumeUnit = null;
        BigDecimal totalVolume = BigDecimal.ZERO;

        for(var shipment: shipments)
        {
            if (!Strings.isNullOrEmpty(shipment.getVolumeUnit()) && !Objects.isNull(shipment.getVolume()))
            {
                if (volumeUnit == null)
                {
                    volumeUnit = shipment.getVolumeUnit();
                }

                if (!shipment.getVolumeUnit().equals(volumeUnit))
                    return Pair.of(BigDecimal.ZERO, null);
                totalVolume = totalVolume.add(shipment.getVolume());
            }
        }
        return Pair.of(totalVolume, volumeUnit);
    }
    public Pair<BigDecimal, String> getTotalPacksManifest(List<ShipmentModel> shipments)
    {
        if (Objects.isNull(shipments) || shipments.isEmpty())
            return Pair.of(BigDecimal.ZERO, null);
        String packsUnit = null;
        BigDecimal totalPacks = BigDecimal.ZERO;

        for(var shipment: shipments)
        {
            if (!Strings.isNullOrEmpty(shipment.getPacksUnit()) && shipment.getNoOfPacks() != null)
            {
                if (packsUnit == null)
                {
                    packsUnit = shipment.getPacksUnit();
                }

                if (!shipment.getPacksUnit().equals(packsUnit))
                    return Pair.of(BigDecimal.ZERO, null);
                totalPacks = totalPacks.add(BigDecimal.valueOf(shipment.getNoOfPacks()));
            }
        }
        return Pair.of(totalPacks, packsUnit);
    }

    public void populateAddress(Map<String, Object> addressData, Map<String, Object> dictionary, String prefix) {
        dictionary.put(prefix + COMPANY_NAME, getValueFromMap(addressData, COMPANY_NAME));
        dictionary.put(prefix + ADDRESS1, getValueFromMap(addressData, ADDRESS1));
        dictionary.put(prefix + ADDRESS2, getValueFromMap(addressData, ADDRESS2));
        dictionary.put(prefix + EMAIL, getValueFromMap(addressData, EMAIL));
        dictionary.put(prefix + CITY, getValueFromMap(addressData, CITY));
        dictionary.put(prefix + STATE, getValueFromMap(addressData, STATE));
        dictionary.put(prefix + COUNTRY, getValueFromMap(addressData, COUNTRY));
        dictionary.put(prefix + CONTACT_PHONE, getValueFromMap(addressData, CONTACT_PHONE));
        dictionary.put(prefix + MOBILE, getValueFromMap(addressData, MOBILE));
        dictionary.put(prefix + ZIP_POST_CODE, getValueFromMap(addressData, ZIP_POST_CODE));
    }

    private Map<String, Object> getAddressForParty(Parties party, Map<String, Object> partiesOrgInfoFromCache) {
        if (party == null) {
            return Collections.emptyMap();
        }
        Map<String, Object> partiesOrgData = (Map<String, Object>) partiesOrgInfoFromCache.get(party.getOrgCode());
        if (partiesOrgData == null) {
            return Collections.emptyMap();
        }
        List<Map<String, Object>> partiesAddressData = (List<Map<String, Object>>) partiesOrgData.get(Constants.ORG_ADDRESS);
        if (partiesAddressData != null) {
            for (Map<String, Object> addressData : partiesAddressData) {
                if (Objects.nonNull(addressData) && Objects.equals(addressData.get(Constants.ADDRESS_SHORT_CODE), party.getAddressCode())) {
                    return addressData;
                }
            }
        }
        return Collections.emptyMap();
    }

    public void populateRaKcData(Map<String, Object> dictionary, ShipmentModel shipmentModel) {
        Parties partiesModelSendingAgent = shipmentModel.getAdditionalDetails().getExportBroker() != null ? modelMapper.map(shipmentModel.getAdditionalDetails().getExportBroker(), Parties.class) : null;
        Parties partiesModelReceivingAgent = shipmentModel.getAdditionalDetails().getImportBroker() != null ? modelMapper.map(shipmentModel.getAdditionalDetails().getImportBroker(), Parties.class) : null;
        Parties consignor = shipmentModel.getConsigner() != null ? modelMapper.map(shipmentModel.getConsigner(), Parties.class) : null;

        List<Parties> parties = Arrays.asList(
                partiesModelSendingAgent,
                partiesModelReceivingAgent,
                consignor
        );
        Map<String, Object> partiesOrgInfoFromCache = masterDataUtils.getPartiesOrgInfoFromCache(parties);

        Map<String, Object> addressSendingAgent = getAddressForParty(partiesModelSendingAgent, partiesOrgInfoFromCache);
        Map<String, Object> addressReceivingAgent = getAddressForParty(partiesModelReceivingAgent, partiesOrgInfoFromCache);
        Map<String, Object> addressConsignorAgent = getAddressForParty(consignor, partiesOrgInfoFromCache);

        processAgent(addressSendingAgent, dictionary, ONE, ORIGIN_AGENT);
        processAgent(addressReceivingAgent, dictionary, ONE, DESTINATION_AGENT);
        processAgent(addressConsignorAgent, dictionary, TWO, CONSIGNOR_AGENT);

        processRaKcAdditionalDetails(dictionary, shipmentModel);

        if(shipmentModel.getSecurityStatus() != null ) {
            dictionary.put(CONSIGNMENT_STATUS, shipmentModel.getSecurityStatus());
        }
    }

    private void processRaKcAdditionalDetails(Map<String, Object> dictionary, ShipmentModel shipmentModel) {
        AdditionalDetailModel additionalDetailModel = shipmentModel.getAdditionalDetails();
        if (additionalDetailModel != null) {
            populateScreeningAndExemptionCodes(dictionary, additionalDetailModel, true);
            dictionary.put(RA_NUMBER, additionalDetailModel.getRegulatedEntityCategory());
            dictionary.put(SECURITY_STATUS_RECEIVED_FROM, additionalDetailModel.getSecurityStatusReceivedFrom());
            dictionary.put(ADDITIONAL_SECURITY_INFORMATION, StringUtility.getNullIfEmpty(additionalDetailModel.getAdditionalSecurityInformation()));
        }
    }

    private void populateScreeningAndExemptionCodes(Map<String, Object> dictionary, AdditionalDetailModel additionalDetailModel, boolean includeAomFreeText) {
        if(additionalDetailModel.getExemptionCodes() != null) {
            dictionary.put(EXEMPTION_CARGO, additionalDetailModel.getExemptionCodes());
        }
        if(additionalDetailModel.getScreeningStatus() != null && !additionalDetailModel.getScreeningStatus().isEmpty()) {
            Set<String> screeningCodes = new HashSet<>(additionalDetailModel.getScreeningStatus());
            if(screeningCodes.contains(Constants.AOM)){
                screeningCodes.remove(Constants.AOM);
                String aomString = Constants.AOM;
                if(additionalDetailModel.getAomFreeText() != null) {
                    aomString =  aomString + " (" + additionalDetailModel.getAomFreeText() + ")";
                }
                screeningCodes.add(aomString);
                dictionary.put(SCREENING_CODES, screeningCodes);
                if(includeAomFreeText) {
                    dictionary.put(AOM_FREE_TEXT, additionalDetailModel.getAomFreeText());
                }
            } else {
                dictionary.put(SCREENING_CODES, screeningCodes);
            }
        }
    }

    public void populateRaKcDataConsolidation(Map<String, Object> dictionary, ConsolidationModel consolidationModel) {
        Parties partiesModelSendingAgent = consolidationModel.getSendingAgent() != null ? modelMapper.map(consolidationModel.getSendingAgent(), Parties.class) : null;

        List<Parties> parties = Collections.singletonList(
                partiesModelSendingAgent
        );

        OrgAddressResponse orgAddressResponse = v1ServiceUtil.fetchOrgInfoFromV1(parties);
        Map<String, Object> addressReceivingAgent = null;

        Map<String, Map<String, Object>> addressMap = orgAddressResponse.getAddresses();
        if(partiesModelSendingAgent != null) {
            addressReceivingAgent = addressMap.get(partiesModelSendingAgent.getOrgCode() + "#" + partiesModelSendingAgent.getAddressCode());
        }

        processAgent(addressReceivingAgent, dictionary, ONE, ORIGIN_AGENT);

        processRaKcConsolidationSendingAgent(dictionary, consolidationModel);
        dictionary.put(ADDITIONAL_SECURITY_INFORMATION, StringUtility.getNullIfEmpty(consolidationModel.getAdditionalSecurityInformation()));

        if(consolidationModel.getSecurityStatus() != null ) {
            dictionary.put(CONSIGNMENT_STATUS, consolidationModel.getSecurityStatus());
        }
    }

    private void processRaKcConsolidationSendingAgent(Map<String, Object> dictionary, ConsolidationModel consolidationModel) {
        if (consolidationModel.getSendingAgent() != null) {

            if(consolidationModel.getExemptionCodes() != null) {
                dictionary.put(EXEMPTION_CARGO, consolidationModel.getExemptionCodes());
            }
            if(consolidationModel.getScreeningStatus() != null && !consolidationModel.getScreeningStatus().isEmpty()) {
                Set<String> screeningCodes = new HashSet<>(consolidationModel.getScreeningStatus());
                if(screeningCodes.contains(Constants.AOM)){
                    screeningCodes.remove(Constants.AOM);
                    String aomString = Constants.AOM;
                    if(consolidationModel.getAomFreeText() != null) {
                        aomString =  aomString + " (" + consolidationModel.getAomFreeText() + ")";
                    }
                    screeningCodes.add(aomString);
                    dictionary.put(SCREENING_CODES, screeningCodes);
                    dictionary.put(AOM_FREE_TEXT, consolidationModel.getAomFreeText());
                } else {
                    dictionary.put(SCREENING_CODES, screeningCodes);
                }

            }
        }
    }

    private String getDate(Map<String, Object> agent) {
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        return convertToDPWDateFormat(LocalDateTime.parse(StringUtility.convertToString(agent.get(KCRA_EXPIRY))), v1TenantSettingsResponse.getDPWDateFormat(), v1TenantSettingsResponse);
    }

    private void processAgent(Map<String, Object> agent, Map<String, Object> dictionary, String type, String agentType) {
        if(agent != null) {
            var raKcType = Boolean.TRUE.equals(agent.get(REGULATED_AGENT))? ONE : "";
            raKcType = Boolean.TRUE.equals(agent.get(KNOWN_CONSIGNOR))? TWO : raKcType;
            if(Objects.equals(raKcType, type)) {
                if(type.equals(ONE)) {
                    dictionary.put(agentType + TYPE, RA);
                } else if(type.equals(TWO)) {
                    dictionary.put(agentType + TYPE, KC);
                }

                processKcraNumber(agent, dictionary, type, agentType);
                processKcraExpiry(agent, dictionary, type, agentType);
            }
        }
    }

    private void processKcraNumber(Map<String, Object> agent, Map<String, Object> dictionary, String type, String agentType) {
        if(StringUtility.isNotEmpty(StringUtility.convertToString(agent.get(KCRA_NUMBER)))) {
            if(type.equals(ONE)) {
                dictionary.put(agentType + RA_NUMBER, agent.get(KCRA_NUMBER));
            } else if(type.equals(TWO)) {
                dictionary.put(agentType + KC_NUMBER, agent.get(KCRA_NUMBER));
            }
        }
    }

    private void processKcraExpiry(Map<String, Object> agent, Map<String, Object> dictionary, String type, String agentType) {
        if(StringUtility.isNotEmpty(StringUtility.convertToString(agent.get(KCRA_EXPIRY)))) {
            if(type.equals(ONE)) {
                dictionary.put(agentType + RA_EXPIRY, getDate(agent));
            } else if(type.equals(TWO)) {
                dictionary.put(agentType + KC_EXPIRY, getDate(agent));
            }
        }
    }

    public void populateIGMInfo(ShipmentModel shipment, Map<String, Object> dictionary) {
        if (Objects.isNull(shipment))
            return;
        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        var additionalDetails = shipment.getAdditionalDetails();
        if (v1TenantSettingsResponse.isEnableIGMDetails() && Objects.equals(shipment.getDirection(), Constants.IMP) && !Objects.isNull(additionalDetails)) {
            dictionary.put(ReportConstants.IGM_FILE_DATE, additionalDetails.getIGMFileDate());
            dictionary.put(ReportConstants.IGM_FILE_NO, additionalDetails.getIGMFileNo());
            dictionary.put(ReportConstants.IGM_INWARD_DATE, additionalDetails.getIGMInwardDate());
            dictionary.put(ReportConstants.INWARD_DATE_TIME, additionalDetails.getInwardDateAndTime());
            dictionary.put(ReportConstants.LINE_NUMBER, additionalDetails.getLineNumber());
            dictionary.put(ReportConstants.SUB_LINE_NUMBER, additionalDetails.getSubLineNumber());
            if (Boolean.TRUE.equals(additionalDetails.getIsInland())) {
                dictionary.put(ReportConstants.IS_INLAND, Boolean.TRUE.equals(additionalDetails.getIsInland()) ? "Yes" : "No");
                dictionary.put(ReportConstants.SMTPIGM_DATE, additionalDetails.getSMTPIGMDate());
                dictionary.put(ReportConstants.SMTPIGM_NUMBER, additionalDetails.getSMTPIGMNumber());
                dictionary.put(ReportConstants.LOCAL_LINE_NUMBER, additionalDetails.getLocalLineNumber());
            }
        }

    }

    private void validateAirDGCheck(ShipmentModel shipmentModel) {
        ShipmentSettingsDetails shipmentSettingsDetails = getCurrentShipmentSettings();
        Boolean countryAirCargoSecurity = shipmentSettingsDetails.getCountryAirCargoSecurity();
        if (!Boolean.TRUE.equals(countryAirCargoSecurity) && shipmentModel.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR)) {
                boolean dgPack = false;
                if(shipmentModel.getPackingList() != null && !shipmentModel.getPackingList().isEmpty()) {
                    for (PackingModel packingModel: shipmentModel.getPackingList()) {
                        if(Boolean.TRUE.equals(packingModel.getHazardous())) {
                            dgPack = true;
                            break;
                        }
                    }
                }
                if(!dgPack) {
                    throw new ValidationException("The shipment is marked as DG but does not contain any DG packages. Please add DG packs before printing.");
                }
            }

    }

    private void validateOceanDGCheck(ShipmentModel shipmentModel) {
        if(shipmentModel.getTransportMode().equals(Constants.TRANSPORT_MODE_SEA)) {
            if(!OceanDGStatus.OCEAN_DG_ACCEPTED.equals(shipmentModel.getOceanDGStatus()) && !OceanDGStatus.OCEAN_DG_COMMERCIAL_ACCEPTED.equals(shipmentModel.getOceanDGStatus()) &&
                    !Constants.IMP.equals(shipmentModel.getDirection())) {
                throw new ValidationException("The shipment is marked as DG but is not approved. Please get the required DG approvals before printing.");
            }
            boolean dgContainer = false;
            if(!listIsNullOrEmpty(shipmentModel.getContainersList())) {
                for (ContainerModel containerModel: shipmentModel.getContainersList()) {
                    if(Boolean.TRUE.equals(containerModel.getHazardous())) {
                        dgContainer = true;
                        break;
                    }
                }
            }
            if(!dgContainer) {
                throw new ValidationException("The shipment is marked as DG but does not contain any DG containers. Please add DG containers before printing.");
            }
        }
    }

    public void validateAirAndOceanDGCheck(ShipmentModel shipmentModel) {
        if(Boolean.TRUE.equals(shipmentModel.getContainsHazardous())) {
            validateAirDGCheck(shipmentModel);
            validateOceanDGCheck(shipmentModel);
        }
    }

    private static boolean isAirDgUser() {
        return UserContext.isAirDgUser();
    }

    private static boolean isAirSecurityUser() {
        return UserContext.isAirSecurityUser();
    }

    public void validateAirDGCheckConsolidations(ConsolidationModel consolidationModel) {
        if(Boolean.TRUE.equals(consolidationModel.getHazardous()) && consolidationModel.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && DIRECTION_EXP.equals(consolidationModel.getShipmentType()) && !isAirDgUser()) {
            throw new ValidationException(ReportConstants.FREIGHT_DOCUMENT_PERMISSION_EXCEPTION);
        }
    }

    public void validateAirDGCheckShipments(ShipmentModel shipmentModel) {
        if(Boolean.TRUE.equals(shipmentModel.getContainsHazardous()) && shipmentModel.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentModel.getDirection().equals(Constants.DIRECTION_EXP) && !isAirDgUser()) {
            throw new ValidationException(ReportConstants.FREIGHT_DOCUMENT_PERMISSION_EXCEPTION);
        }
    }

    public void updateShipmentWeightAndPack(Map<String, Object> dictionary, V1TenantSettingsResponse v1TenantSettingsResponse) {
        if(dictionary.get(ReportConstants.SHIPMENTS) != null) {
            List<Map<String, Object>> values = jsonHelper.convertValue(dictionary.get(ReportConstants.SHIPMENTS), new TypeReference<>() {});
            if (Objects.isNull(values)) values = new ArrayList<>();
            values.forEach(v -> {
                if (v.containsKey(ReportConstants.WEIGHT))
                    v.put(ReportConstants.WEIGHT, convertToWeightNumberFormat(new BigDecimal(v.get(ReportConstants.WEIGHT).toString()), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.TOTAL_PACKS))
                    v.put(ReportConstants.TOTAL_PACKS, convertToVolumeNumberFormat(new BigDecimal(v.get(ReportConstants.TOTAL_PACKS).toString()), v1TenantSettingsResponse));
                if (v.containsKey(ReportConstants.DESCRIPTION))
                    v.put(ReportConstants.DESCRIPTION, StringUtility.toUpperCase(StringUtility.convertToString(v.get(ReportConstants.DESCRIPTION))));
            });
            dictionary.put(ReportConstants.SHIPMENTS, values);
        }
    }

    public void addTransportInstructionTags(Map<String, Object> dictionary, ShipmentModel shipmentModel) {
        if (Objects.isNull(shipmentModel.getPickupDeliveryDetailsInstructions()) || shipmentModel.getPickupDeliveryDetailsInstructions().isEmpty())
            return;

        Optional<PickupDeliveryDetailsModel> transportInstruction = shipmentModel.getPickupDeliveryDetailsInstructions().stream().filter(pickupDeliveryDetailsModel -> pickupDeliveryDetailsModel.getId().equals(shipmentModel.getTransportInstructionId())).findFirst();
        if (transportInstruction.isEmpty())
            return;
        var ti = transportInstruction.get();
        Optional<PartiesModel> exportAgent = getPartyAgent(ti, "EXA");
        Optional<PartiesModel> importAgent = getPartyAgent(ti, "IMA");
        Optional<PartiesModel> deliveryAgent = getPartyAgent(ti, "DAG");
        dictionary.put(TI_INSTRUCTIONTYPE, ti.getType());
        dictionary.put(TI_DROPMODE, ti.getDropMode());

        addTIAgentTags(dictionary, exportAgent, importAgent, deliveryAgent);

        V1TenantSettingsResponse v1TenantSettingsResponse = getCurrentTenantSettings();
        String tsDateTimeFormat = v1TenantSettingsResponse.getDPWDateFormat();

        dictionary.put(TI_TRANSPORTCOMPANY, ti.getTransporterDetail() != null && ti.getTransporterDetail().getOrgData() != null ? ti.getTransporterDetail().getOrgData().get(FULL_NAME) : "");
        dictionary.put(TI_PICKUPFROM, ti.getSourceDetail() != null && ti.getSourceDetail().getOrgData() != null ? ti.getSourceDetail().getOrgData().get(FULL_NAME) : "");
        dictionary.put(TI_DELIVERTO, ti.getDestinationDetail() != null && ti.getDestinationDetail().getOrgData() != null ? ti.getDestinationDetail().getOrgData().get(FULL_NAME) : "");
        dictionary.put(TI_TRANSPORTCOMPANYADDRESS, getFormattedAddress(ti.getTransporterDetail(),false));
        dictionary.put(TI_TRANSPORTCOMPANYCONTACT, ti.getTransporterDetail() != null ? ReportHelper.getValueFromMap(ti.getTransporterDetail().getAddressData(), CONTACT_PHONE) : "");
        dictionary.put(TI_PICKUPFROMADDRESS, getFormattedAddress(ti.getSourceDetail(),false));
        dictionary.put(TI_PICKUPFROMCONTACT, ti.getSourceDetail() != null ? ReportHelper.getValueFromMap(ti.getSourceDetail().getAddressData(), CONTACT_PHONE) : "");
        dictionary.put(TI_DELIVERTOADDRESS, getFormattedAddress(ti.getDestinationDetail(),false));
        dictionary.put(TI_DELIVERTOCONTACT, ti.getDestinationDetail() != null ? ReportHelper.getValueFromMap(ti.getDestinationDetail().getAddressData(), CONTACT_PHONE) : "");
        dictionary.put(TI_REMARKS, ti.getRemarks());
        dictionary.put(TI_PORTTRANSPORTADVISED, convertToDPWDateFormat(ti.getPortTransportAdvised(), tsDateTimeFormat, true));
        dictionary.put(TI_REQUIREDBY, convertToDPWDateFormat(ti.getRequiredBy(), tsDateTimeFormat, true));
        dictionary.put(TI_ESTIMATEDPICKUP, convertToDPWDateFormat(ti.getEstimatedPickup() , tsDateTimeFormat, true));
        dictionary.put(TI_ESTIMATEDDELIVERY, convertToDPWDateFormat(ti.getEstimatedDelivery() , tsDateTimeFormat, true));
        dictionary.put(TI_ACTUALPICKUP, convertToDPWDateFormat(ti.getActualPickup() , tsDateTimeFormat, true));
        dictionary.put(TI_ACTUALDELIVERY, convertToDPWDateFormat(ti.getActualDelivery() , tsDateTimeFormat, true));
        dictionary.put(TI_PICKUP_GATEIN, convertToDPWDateFormat(ti.getPickupGateIn() , tsDateTimeFormat, true));
        dictionary.put(TI_PICKUP_GATEOUT, convertToDPWDateFormat(ti.getPickupGateOut() , tsDateTimeFormat, true));
        dictionary.put(TI_DELIVERY_GATEIN, convertToDPWDateFormat(ti.getDeliveryGateIn() , tsDateTimeFormat, true));
        dictionary.put(TI_DELIVERY_GATEOUT, convertToDPWDateFormat(ti.getDeliveryGateOut() , tsDateTimeFormat, true));
        addScreeningCodeAndExemptionCargoTags(dictionary, shipmentModel);
        if(shipmentModel.getSecurityStatus() != null ) {
            dictionary.put(CONSIGNMENT_STATUS, shipmentModel.getSecurityStatus());
        }
    }

    private void addTIAgentTags(Map<String, Object> dictionary, Optional<PartiesModel> exportAgent, Optional<PartiesModel> importAgent, Optional<PartiesModel> deliveryAgent) {
        dictionary.put(ReportConstants.TI_EXPORT_AGENT, exportAgent.isPresent() && exportAgent.get().getOrgData() != null ? exportAgent.get().getOrgData().get(FULL_NAME) : "");
        dictionary.put(ReportConstants.TI_EXPORT_AGENT_ADDRESS, exportAgent.isPresent() ? getFormattedAddress(exportAgent.get(),false) : "");
        dictionary.put(ReportConstants.TI_EXPORT_AGENT_CONTACT,  exportAgent.isPresent() ? ReportHelper.getValueFromMap(exportAgent.get().getAddressData(), ReportConstants.CONTACT_PHONE) : "");

        dictionary.put(ReportConstants.TI_IMPORT_AGENT , importAgent.isPresent() && importAgent.get().getOrgData() != null ? importAgent.get().getOrgData().get(FULL_NAME) : "");
        dictionary.put(ReportConstants.TI_IMPORT_AGENT_ADDRESS, importAgent.isPresent() ? getFormattedAddress(importAgent.get(),false) : "");
        dictionary.put(ReportConstants.TI_IMPORT_AGENT_CONTACT,  importAgent.isPresent() ? ReportHelper.getValueFromMap(importAgent.get().getAddressData(), ReportConstants.CONTACT_PHONE) : "");

        dictionary.put(TI_DELIVERY_AGENT, deliveryAgent.isPresent() && deliveryAgent.get().getOrgData() != null ? deliveryAgent.get().getOrgData().get(FULL_NAME) : "");
        dictionary.put(TI_DELIVERY_AGENT_ADDRESS, deliveryAgent.isPresent() ? getFormattedAddress(deliveryAgent.get(),false) : "");
        dictionary.put(TI_DELIVERY_AGENT_CONTACT, deliveryAgent.isPresent() ? ReportHelper.getValueFromMap(deliveryAgent.get().getAddressData(), ReportConstants.CONTACT_PHONE) : "");
    }

    private void addScreeningCodeAndExemptionCargoTags(Map<String, Object> dictionary, ShipmentModel shipmentModel) {
        AdditionalDetailModel details = shipmentModel.getAdditionalDetails();
        if (details != null) {
            populateScreeningAndExemptionCodes(dictionary, details, false);
        }
    }

    private Optional<PartiesModel> getPartyAgent(PickupDeliveryDetailsModel ti, String agentType) {
        return ti.getPartiesList() != null ? ti.getPartiesList().stream().filter(Objects::nonNull).filter(c -> c.getType().toUpperCase().contains(agentType)).findFirst() : Optional.empty();
    }

    public V1TenantSettingsResponse getCurrentTenantSettings() {
        return commonUtils.getCurrentTenantSettings();
    }

    public ShipmentSettingsDetails getCurrentShipmentSettings() {
        return commonUtils.getShipmentSettingFromContext();
    }

    public String getDefaultRANumber() {
        String defaultRANumber = null;
        try {
            TenantModel tenantModel = getTenant();
            CommonV1ListRequest commonV1ListRequest = new CommonV1ListRequest();
            commonV1ListRequest.setCriteriaRequests(List.of(
                    List.of("Id"),
                    "=",
                    tenantModel.getDefaultAddressId()
            ));
            V1DataResponse response = v1Service.addressList(commonV1ListRequest);
            List<AddressDataV1> addressDataList = jsonHelper.convertValueToList(response.getEntities(), AddressDataV1.class);
            defaultRANumber = Optional.of(addressDataList.get(0)).map(AddressDataV1::getKcraNumber).orElse(null);
        }
        catch (Exception e) {
            log.error("Error while getting RA Number for tenant's default address");
        }
        return defaultRANumber;
    }

    public String geteCSDInfo(Awb awb) {

        if (StringUtility.isEmpty(awb.getAwbCargoInfo().getRaNumber()))
            return Constants.EMPTY_STRING;

        List<String> eCsdInfoList = new ArrayList<>();
        eCsdInfoList.add(awb.getAwbCargoInfo().getCountryCode());
        eCsdInfoList.add(ReportConstants.RA);
        eCsdInfoList.add(awb.getAwbCargoInfo().getRaNumber());
        if (!CommonUtils.listIsNullOrEmpty(awb.getAwbCargoInfo().getScreeningStatus())) {
            eCsdInfoList.add(String.join("/", awb.getAwbCargoInfo().getScreeningStatus().stream()
                    .map(c -> Objects.equals(c, Constants.AOM) ? awb.getAwbCargoInfo().getOtherMethod() : c)
                    .toList()));
        }
        eCsdInfoList.add(Objects.equals(awb.getAwbCargoInfo().getSecurityStatus(), AwbConstants.EXEMPTION_CARGO_SECURITY_STATUS) ? AwbConstants.SPX : awb.getAwbCargoInfo().getSecurityStatus());
        eCsdInfoList.add(awb.getAwbCargoInfo().getExemptionCode());

        eCsdInfoList.add(getPrintOriginalDate(awb));

        eCsdInfoList.add(AwbUtility.getScreenersName(awb));

        return String.join("/", eCsdInfoList.stream().filter(StringUtility::isNotEmpty).toList());
    }

    public String getPrintOriginalDate(Awb awb) {
        if (Objects.nonNull(awb.getAwbCargoInfo().getScreeningTime()))
            return (StringUtility.toUpperCase(convertToDPWDateFormat(awb.getAwbCargoInfo().getScreeningTime(), "ddMMMyy HHmm", true)));
        else if (Objects.nonNull(awb.getOriginalPrintedAt()))
            return (StringUtility.toUpperCase(convertToDPWDateFormat(awb.getOriginalPrintedAt(), "ddMMMyy HHmm", true)));
        return Constants.EMPTY_STRING;
    }

    public void populateRaKcDataWithShipmentDetails(Map<String, Object> dictionary, ShipmentDetails shipmentDetails) {
        Parties partiesModelSendingAgent = shipmentDetails.getAdditionalDetails().getExportBroker() != null ? modelMapper.map(shipmentDetails.getAdditionalDetails().getExportBroker(), Parties.class) : null;
        Parties partiesModelReceivingAgent = shipmentDetails.getAdditionalDetails().getImportBroker() != null ? modelMapper.map(shipmentDetails.getAdditionalDetails().getImportBroker(), Parties.class) : null;
        Parties consignor = shipmentDetails.getConsigner() != null ? modelMapper.map(shipmentDetails.getConsigner(), Parties.class) : null;

        List<Parties> parties = Arrays.asList(
                partiesModelSendingAgent,
                partiesModelReceivingAgent,
                consignor
        );
        Map<String, Object> partiesOrgInfoFromCache = masterDataUtils.getPartiesOrgInfoFromCache(parties);

        Map<String, Object> addressSendingAgent = getAddressForParty(partiesModelSendingAgent, partiesOrgInfoFromCache);
        Map<String, Object> addressReceivingAgent = getAddressForParty(partiesModelReceivingAgent, partiesOrgInfoFromCache);
        Map<String, Object> addressConsignorAgent = getAddressForParty(consignor, partiesOrgInfoFromCache);

        processAgent(addressSendingAgent, dictionary, ONE, ORIGIN_AGENT);
        processAgent(addressReceivingAgent, dictionary, ONE, DESTINATION_AGENT);
        processAgent(addressConsignorAgent, dictionary, TWO, CONSIGNOR_AGENT);

        if (shipmentDetails.getAdditionalDetails() != null) {
            AdditionalDetails additionalDetailModel = shipmentDetails.getAdditionalDetails();
            if(additionalDetailModel.getExemptionCodes() != null) {
                dictionary.put(EXEMPTION_CARGO, additionalDetailModel.getExemptionCodes());
            }
            if(additionalDetailModel.getScreeningStatus() != null && !additionalDetailModel.getScreeningStatus().isEmpty()) {
                addTagsForScreeningStatus(dictionary, additionalDetailModel);
            }

            dictionary.put(RA_NUMBER, additionalDetailModel.getRegulatedEntityCategory());
            dictionary.put(SECURITY_STATUS_RECEIVED_FROM, additionalDetailModel.getSecurityStatusReceivedFrom());
            dictionary.put(ADDITIONAL_SECURITY_INFORMATION, StringUtility.getNullIfEmpty(additionalDetailModel.getAdditionalSecurityInformation()));
        }

        if(shipmentDetails.getSecurityStatus() != null ) {
            dictionary.put(CONSIGNMENT_STATUS, shipmentDetails.getSecurityStatus());
        }
    }

    private void addTagsForScreeningStatus(Map<String, Object> dictionary, AdditionalDetails additionalDetailModel) {
        Set<String> screeningCodes = additionalDetailModel.getScreeningStatus().stream().collect(Collectors.toSet());
        if(screeningCodes.contains(Constants.AOM)){
            screeningCodes.remove(Constants.AOM);
            String aomString = Constants.AOM;
            if(additionalDetailModel.getAomFreeText() != null) {
                aomString =  aomString + " (" + additionalDetailModel.getAomFreeText() + ")";
            }
            screeningCodes.add(aomString);
            dictionary.put(SCREENING_CODES, screeningCodes);
            dictionary.put(AOM_FREE_TEXT, additionalDetailModel.getAomFreeText());
        } else {
            dictionary.put(SCREENING_CODES, screeningCodes);
        }
    }

    public void validateAirDGAndAirSecurityCheckShipments(ShipmentModel shipmentModel) {
        if (shipmentModel.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentModel.getDirection().equals(Constants.DIRECTION_EXP)) {
            if (!isAirSecurityUser()) {
                throw new ValidationException(FREIGHT_DOCUMENT_PERMISSION_EXCEPTION);
            }
            if (Boolean.TRUE.equals(shipmentModel.getContainsHazardous()) && !isAirDgUser()) {
                throw new ValidationException(FREIGHT_DOCUMENT_PERMISSION_EXCEPTION);
            }
        }
    }

    public void validateAirSecurityCheckShipments(ShipmentModel shipmentModel) {
        if (shipmentModel.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && shipmentModel.getDirection().equals(Constants.DIRECTION_EXP) && !isAirSecurityUser()) {
            throw new ValidationException(FREIGHT_DOCUMENT_PERMISSION_EXCEPTION);
        }
    }

    public void validateAirDGAndAirSecurityCheckConsolidations(ConsolidationModel consolidationModel) {
        if (consolidationModel.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && consolidationModel.getShipmentType().equals(Constants.DIRECTION_EXP)) {
            if (!isAirSecurityUser()) {
                throw new ValidationException(FREIGHT_DOCUMENT_PERMISSION_EXCEPTION);
            }
            if (Boolean.TRUE.equals(consolidationModel.getHazardous()) && !isAirDgUser()) {
                throw new ValidationException(FREIGHT_DOCUMENT_PERMISSION_EXCEPTION);
            }
        }
    }

    public void validateAirSecurityCheckConsolidations(ConsolidationModel consolidationModel) {
        if (consolidationModel.getTransportMode().equals(Constants.TRANSPORT_MODE_AIR) && consolidationModel.getShipmentType().equals(Constants.DIRECTION_EXP) && !isAirSecurityUser()) {
            throw new ValidationException(FREIGHT_DOCUMENT_PERMISSION_EXCEPTION);
        }
    }

    // Main orchestrator method that populates the data dump dictionary with all required details
    public void populateConsolidationReportData(Map<String, Object> dict, ConsolidationDetails consolidationDetails, Long consolId) {
        if (consolId == null && consolidationDetails == null) {
            return;
        }

        if (consolId != null) {
            consolidationDetails = consolidationDetailsDao.findConsolidationsById(consolId);
        }

        if (dict == null) {
            dict = new HashMap<>();
        }

        List<Parties> partiesList = Stream.of(
                        consolidationDetails.getBorrowedFrom(),
                        consolidationDetails.getCreditor(),
                        consolidationDetails.getCoLoadWith())
                .filter(Objects::nonNull)
                .collect(Collectors.toCollection(ArrayList::new));

        Optional.ofNullable(consolidationDetails.getConsolidationAddresses())
                .ifPresent(partiesList::addAll);

        // Add various grouped information into the map
        addBasicConsolidationFields(dict, consolidationDetails);
        addReferenceNumbers(dict, consolidationDetails.getReferenceNumbersList());
        addRoutingDetails(dict, consolidationDetails.getRoutingsList());
        addPartyDetails(dict, partiesList);
        addAgentDetails(dict, C_ORIGIN_AGENT, consolidationDetails.getSendingAgent());
        addAgentDetails(dict, C_DESTINATION_AGENT, consolidationDetails.getReceivingAgent());
        addBranchAndTriangulationDetails(dict, consolidationDetails);
    }

    // Adds simple scalar fields and nested allocation/quantity-related values
    private void addBasicConsolidationFields(Map<String, Object> dict, ConsolidationDetails details) {
        dict.put(ReportConstants.C_D_REEFER, details.getReefer());
        dict.put(ReportConstants.C_D_DG, details.getHazardous());
        dict.put(ReportConstants.C_D_INCOTERM, details.getIncoterms());

        // Add container and package counts from allocation section if available
        Allocations al = details.getAllocations();
        if (al != null) {
            dict.put(ReportConstants.C_CA_DGCONTAINER, al.getDgContainerCount());
            dict.put(ReportConstants.C_CA_DGPACKAGES, al.getDgPacks());
        }

        // Add achieved quantities section if available
        AchievedQuantities aq = details.getAchievedQuantities();
        if (aq != null) {
            dict.put(ReportConstants.C_C_DGPACKAGESTYPE, aq.getDgPacksType());
            dict.put(ReportConstants.C_C_DGCONTAINER, aq.getDgContainerCount());
            dict.put(ReportConstants.C_C_DGPACKAGES, aq.getDgPacks());
            dict.put(ReportConstants.C_C_SLACCOUNT, aq.getSlacCount());
        }

        dict.put(ReportConstants.C_C_ADDITIONAL_TERMS, details.getAdditionalTerms()); // terms
    }

    // Adds reference numbers into the map using their type as a key suffix
    private void addReferenceNumbers(Map<String, Object> dict, List<ReferenceNumbers> refs) {
        if (refs == null) {
            return;
        }

        for (ReferenceNumbers ref : refs) {
            if (ref != null && ref.getType() != null) {
                dict.put("C_" + ref.getType().replaceAll("\\s+", ""), ref.getReferenceNumber());
            }
        }
    }

    // Adds first and last routing information from MAIN_CARRIAGE legs only
    private void addRoutingDetails(Map<String, Object> dict, List<Routings> routings) {
        if (routings == null || routings.isEmpty()) {
            return;
        }

        // Filter only main carriage routes
        List<Routings> main = routings.stream()
                .filter(r -> r != null && r.getCarriage() == RoutingCarriage.MAIN_CARRIAGE)
                .toList();

        if (main.isEmpty()) {
            return;
        }

        Routings first = main.get(0);
        Routings last = main.get(main.size() - 1);

        // Add last routing info
        if (last != null) {
            dict.put(ReportConstants.C_LASTVESSEL, last.getVesselName());
            dict.put(ReportConstants.C_LASTVOYAGE, last.getVoyage());
            dict.put(ReportConstants.C_LASTCARRIER, last.getCarrier());
            dict.put(ReportConstants.C_LASTFLIGHTNUMBER, last.getFlightNumber());
        }

        // Add first routing info
        if (first != null) {
            dict.put(ReportConstants.C_FIRSTVESSEL, first.getVesselName());
            dict.put(ReportConstants.C_FIRSTVOYAGE, first.getVoyage());
            dict.put(ReportConstants.C_FIRSTCARRIER, first.getCarrier());
            dict.put(ReportConstants.C_FIRSTFLIGHTNUMBER, first.getFlightNumber());
        }
    }

    // Adds each party's mapped data using their type (like SHIPPER, CONSIGNEE) as the key
    private void addPartyDetails(Map<String, Object> dict, List<Parties> parties) {
        if (parties == null) {
            return;
        }

        for (Parties party : parties) {
            if (party != null && party.getType() != null) {
                dict.put("C_" + party.getType().replaceAll("\\s+", ""), buildPartyMap(party));
                dict.put("C_" + party.getType().replaceAll("\\s+", "") + CONTACT, buildPartyContact(party));
            }
        }
    }

    // Adds single agent party (either origin or destination) using a provided key
    private void addAgentDetails(Map<String, Object> dict, String key, Parties agent) {
        if (agent != null) {
            dict.put(key, buildPartyMap(agent));
            dict.put(key + CONTACT, buildPartyContact(agent));
        }
    }

    // Converts a Parties object into a consistent map of address/organization values
    private List<String> buildPartyMap(Parties party) {
        if (party == null) {
            return List.of();
        }

        // Extract values from party, safely
        String orgName = party.getOrgData() != null
                ? (String) party.getOrgData().get(PartiesConstants.FULLNAME)
                : null;

        Map<String, Object> addressData = party.getAddressData();
        String address1 = addressData != null ? (String) addressData.get(PartiesConstants.ADDRESS1) : null;
        String address2 = addressData != null ? (String) addressData.get(PartiesConstants.ADDRESS2) : null;
        String city = addressData != null ? (String) addressData.get(PartiesConstants.CITY) : null;
        String state = addressData != null ? (String) addressData.get(PartiesConstants.STATE) : null;
        String zip = addressData != null ? (String) addressData.get(PartiesConstants.ZIP_POST_CODE) : null;
        String country = null;
        if (addressData != null) {
            String countryCode = (String) addressData.get(PartiesConstants.COUNTRY);
            String countryName = ISO3166.getCountryNameByCode(countryCode != null ? countryCode : "");
            country = !Constants.EMPTY_STRING.equals(countryName) ? countryName : countryCode;
        }

        // Use getFormattedDetails to format all values
        List<String> formatted = getFormattedDetailsV2(
                orgName, address1, address2, country, state,
                city, zip, null
        );

        // Uppercase all non-null formatted values safely
        return Optional.ofNullable(formatted).orElse(List.of())
                .stream().map(value -> value != null ? value.toUpperCase() : null).toList();
    }

    private List<String> buildPartyContact(Parties party) {
        if (party == null) {
            return List.of();
        }

        String email = party.getOrgData() != null
                ? (String) party.getOrgData().get(PartiesConstants.EMAIL) : null;

        String phone = party.getOrgData() != null
                ? (String) party.getOrgData().get(PartiesConstants.PHONE) : null;

        List<String> formatted = addEmailAndPhoneDetails(email, "Email: ", phone, "Phone: ");

        // Uppercase all non-null formatted values safely
        return Optional.ofNullable(formatted).orElse(List.of())
                .stream().map(value -> value != null ? value.toUpperCase() : null).toList();
    }

    // Adds origin/receiving branches and triangulated partner branch info
    private void addBranchAndTriangulationDetails(Map<String, Object> dict, ConsolidationDetails consolidationDetails) {
        Long origin = consolidationDetails.getOriginBranch();
        Long receiving = consolidationDetails.getReceivingBranch();
        List<TriangulationPartner> triangulations = consolidationDetails.getTriangulationPartnerList();
        populateBranchAndTriangulationInfo(dict, origin, receiving, triangulations, "C" );
    }

    // Builds a map from a tenant's address and name info
    private List<String> buildTenantMap(TenantModel tenant) {
        if (tenant == null) {
            return List.of();
        }

        List<String> formattedDetails = getFormattedDetails(tenant.getDisplayName(), tenant.getAddress1(), tenant.getAddress2(), tenant.getCountry(), tenant.getState(),
                tenant.getCity(),
                tenant.getZipPostCode(), null);

        return Optional.ofNullable(formattedDetails).orElse(List.of())
                .stream().map(value -> value != null ? value.toUpperCase() : null).toList();
    }

    private List<String> buildTenantContact(TenantModel tenantModel) {
        return Optional.ofNullable(tenantModel)
                .map(tenant -> {
                    String email = tenant.getEmail();
                    String phone = tenant.getPhone();

                    return addEmailAndPhoneDetails(email, "Email: ", phone, "Phone: ")
                            .stream().map(value -> value != null ? value.toUpperCase() : null).toList();
                })
                .orElse(List.of());
    }


    // Main orchestrator method that populates the data dump dictionary with all required details
    public void populateShipmentReportData(Map<String, Object> dict, ShipmentDetails shipmentDetails, Long shipmentId) {
        if (shipmentId == null && shipmentDetails == null) {
            return;
        }

        if (shipmentId != null) {
            Optional<ShipmentDetails> byId = shipmentDao.findById(shipmentId);
            if (byId.isPresent()) {
                shipmentDetails = byId.get();
            }
        }

        if (dict == null) {
            dict = new HashMap<>();
        }

        List<Parties> partiesList = Stream.of(
                        shipmentDetails.getClient(),
                        shipmentDetails.getConsignee(),
                        shipmentDetails.getConsigner(),
                        shipmentDetails.getAdditionalDetails() != null
                                ? shipmentDetails.getAdditionalDetails().getNotifyParty()
                                : null
                )
                .filter(Objects::nonNull)
                .collect(Collectors.toCollection(ArrayList::new));

        Optional.ofNullable(shipmentDetails.getShipmentAddresses())
                .ifPresent(partiesList::addAll);

        // Add various grouped information into the map
        addBasicShipmentFields(dict, shipmentDetails);
        addShipmentReferenceNumbers(dict, shipmentDetails.getReferenceNumbersList());
        addShipmentRoutingDetails(dict, shipmentDetails.getRoutingsList());
        addShipmentPartyDetails(dict, partiesList);
        addShipmentAgentDetails(dict, "S_OriginAgent", shipmentDetails.getAdditionalDetails().getSendingAgent());
        addShipmentAgentDetails(dict, "S_DestinationAgent", shipmentDetails.getAdditionalDetails().getReceivingAgent());
        addShipmentBranchAndTriangulationDetails(dict, shipmentDetails);
    }

    // Adds simple scalar fields and nested allocation/quantity-related values
    private void addBasicShipmentFields(Map<String, Object> dictionary, ShipmentDetails details) {
        Map<String, Object> masterDataMap = shipmentServiceImplV3.getAllMasterData(details.getId(), SHIPMENT);
        Map<String, String> orderDpwMap = extractOrderDpwMap(masterDataMap);
        Map<String, String> orgMap = extractOrgMap(masterDataMap);

        addControlFields(dictionary, details);
        addPartnerFields(dictionary, details, orderDpwMap, orgMap);
        addCutoffFields(dictionary, details);
        addAdditionalFields(dictionary, details);
    }

    private Map<String, String> extractOrderDpwMap(Map<String, Object> masterDataMap) {
        if (masterDataMap == null) return null;

        Map<String, Map<String, String>> masterList =
                (Map<String, Map<String, String>>) masterDataMap.get(CacheConstants.MASTER_LIST);

        return masterList != null ? masterList.get(CacheConstants.ORDER_DPW) : null;
    }

    private Map<String, String> extractOrgMap(Map<String, Object> masterDataMap) {
        return masterDataMap != null
                ? (Map<String, String>) masterDataMap.get(CacheConstants.ORGANIZATIONS)
                : null;
    }

    private void addControlFields(Map<String, Object> dictionary, ShipmentDetails details) {
        dictionary.put(S_CONTROLLED, details.getControlled());
        dictionary.put(S_CONTROLLED_REF_NO, details.getControlledReferenceNumber());
        dictionary.put(S_INCOTERM_LOCATION, details.getIncotermsLocation());
        dictionary.put(S_REEFER_FLAG, details.getIsReefer());
    }

    private void addPartnerFields(Map<String, Object> dictionary, ShipmentDetails details,
                                  Map<String, String> orderDpwMap, Map<String, String> orgMap) {
        dictionary.put(S_PARTNER_DROP_DOWN,
                orderDpwMap != null ? orderDpwMap.keySet().iterator().next() : null);

        String bookingAgentName = getOrgValue(orgMap, details.getBookingAgent());
        dictionary.put(S_CO_LOADER_NAME, bookingAgentName);
        dictionary.put(S_BOOKING_AGENT, bookingAgentName);

        // Co-loader fields
        dictionary.put(S_CO_LOADER_BKG_NO, details.getCoLoadBkgNumber());
        dictionary.put(S_CO_LOADER_BL_NO, details.getCoLoadBlNumber());
        dictionary.put(S_CO_LOADER_AWB_NO, details.getCoLoadBlNumber());

        // Booking agent fields (same values as co-loader)
        dictionary.put(S_BOOKING_AGENT_BKG_NO, details.getCoLoadBkgNumber());
        dictionary.put(S_BOOKING_AGENT_BL_NO, details.getCoLoadBlNumber());
        dictionary.put(S_BOOKING_AGENT_AWB_NO, details.getCoLoadBlNumber());

        // Location fields
        dictionary.put(S_PICKUP_AT_ORIGIN,
                getOrgValue(orgMap, details.getPickupAtOrigin()));
        dictionary.put(S_DELIVERY_AT_DESTINATION,
                getOrgValue(orgMap, details.getDeliveryAtDestination()));
        dictionary.put(S_CUSTOM_BROKERAGE_AT_ORIGIN,
                getOrgValue(orgMap, details.getBrokerageAtOrigin()));
        dictionary.put(S_CUSTOM_BROKERAGE_AT_DESTINATION,
                getOrgValue(orgMap, details.getBrokerageAtDestination()));
    }

    private String getOrgValue(Map<String, String> orgMap, Object id) {
        return orgMap != null && id != null ? orgMap.get(id.toString()) : null;
    }

    private void addCutoffFields(Map<String, Object> dictionary, ShipmentDetails details) {
        dictionary.put(S_TERMINAL, convertToDPWDateFormat(details.getTerminalCutoff(), Constants.DATE_TIME_FORMAT_DDMMMYYYY_HHMM, false));
        dictionary.put(S_VGM, convertToDPWDateFormat(details.getVerifiedGrossMassCutoff(), Constants.DATE_TIME_FORMAT_DDMMMYYYY_HHMM, false));
        dictionary.put(S_SI, convertToDPWDateFormat(details.getShippingInstructionCutoff(), Constants.DATE_TIME_FORMAT_DDMMMYYYY_HHMM, false));
        dictionary.put(S_EAR_EPY_EQ_PICK, convertToDPWDateFormat(details.getEarliestEmptyEquipmentPickUp(), Constants.DATE_TIME_FORMAT_DDMMMYYYY_HHMM, false));
        dictionary.put(S_LAT_FULL_EQ_DELI, details.getLatestFullEquipmentDeliveredToCarrier());
        dictionary.put(S_EAR_DROP_OFF, details.getEarliestDropOffFullEquipmentToCarrier());
        dictionary.put(S_REEFER, convertToDPWDateFormat(details.getReeferCutoff(), Constants.DATE_TIME_FORMAT_DDMMMYYYY_HHMM, false));
        dictionary.put(S_DG, convertToDPWDateFormat(details.getDgCutoff(), Constants.DATE_TIME_FORMAT_DDMMMYYYY_HHMM, true));
        dictionary.put(S_LAT, convertToDPWDateFormat(details.getLatestArrivalTime(), Constants.DATE_TIME_FORMAT_DDMMMYYYY_HHMM, true));
    }

    private void addAdditionalFields(Map<String, Object> dictionary, ShipmentDetails details) {
        AdditionalDetails additional = details.getAdditionalDetails();
        if (additional == null) {
            addNullAdditionalFields(dictionary);
            return;
        }

        dictionary.put(S_BOE_NUMBER, additional.getBOENumber());
        dictionary.put(S_BOE_DATE, convertToDPWDateFormat(additional.getBOEDate()));
        List<String> tenantPartyValueList = buildPartyMap(additional.getOwnershipOrg());
        dictionary.put(S_OWNERSHIP_NAME, !tenantPartyValueList.isEmpty() ? tenantPartyValueList.get(0) : "");
        dictionary.put(S_PASSED_BY_PERSON, additional.getPassedByPerson());
        List<String> partyValueList = buildPartyMap(additional.getBorrowedFrom());
        dictionary.put(S_BORROWED_FROM, !partyValueList.isEmpty() ? partyValueList.get(0) : "");

        dictionary.put(S_OWNERSHIP,
                additional.getOwnership() != null ? additional.getOwnership().getDescription() : null);
        dictionary.put(S_PASSED_BY,
                additional.getPassedBy() != null ? additional.getPassedBy().getDescription() : null);
    }

    private void addNullAdditionalFields(Map<String, Object> dictionary) {
        dictionary.put(S_BOE_NUMBER, null);
        dictionary.put(S_BOE_DATE, null);
        dictionary.put(S_OWNERSHIP, null);
        dictionary.put(S_OWNERSHIP_NAME, null);
        dictionary.put(S_PASSED_BY, null);
        dictionary.put(S_PASSED_BY_PERSON, null);
    }

    // Adds reference numbers into the map using their type as a key suffix
    private void addShipmentReferenceNumbers(Map<String, Object> dict, List<ReferenceNumbers> refs) {
        if (refs == null) {
            return;
        }

        for (ReferenceNumbers ref : refs) {
            if (ref != null && ref.getType() != null) {
                dict.put("S_" + ref.getType().replaceAll("\\s+", ""), ref.getReferenceNumber());
            }
        }
    }

    // Adds first and last routing information from MAIN_CARRIAGE legs only
    private void addShipmentRoutingDetails(Map<String, Object> dict, List<Routings> routings) {
        if (routings == null || routings.isEmpty()) {
            return;
        }

        // Filter main & pre carriage routes
        List<Routings> main = routings.stream()
                .filter(r -> r != null && r.getCarriage() == RoutingCarriage.MAIN_CARRIAGE)
                .toList();

        if (ObjectUtils.isNotEmpty(main)) {
            Routings firstMain = main.get(0);
            Routings lastMain = main.get(main.size() - 1);

            // Add last main routing info
            if (lastMain != null) {
                dict.put(S_LAST_VESSEL, lastMain.getVesselName());
                dict.put(S_LAST_VOYAGE, lastMain.getVoyage());
                dict.put(S_LAST_CARRIER, lastMain.getCarrier());
                dict.put(S_LAST_FLIGHT_NUMBER, lastMain.getFlightNumber());
            }

            // Add first main routing info
            if (firstMain != null) {
                dict.put(S_FIRST_VESSEL, firstMain.getVesselName());
                dict.put(S_FIRST_VOYAGE, firstMain.getVoyage());
                dict.put(S_FIRST_CARRIER, firstMain.getCarrier());
                dict.put(S_FIRST_FLIGHT_NUMBER, firstMain.getFlightNumber());
            }

        }

        List<Routings> pre = routings.stream()
                .filter(r -> r != null && r.getCarriage() == RoutingCarriage.PRE_CARRIAGE)
                .toList();

        if (ObjectUtils.isNotEmpty(pre)) {
            Routings firstPre = pre.get(0);
            Routings lastPre = pre.get(pre.size() - 1);

            // Add last pre routing info
            if (lastPre != null) {
                dict.put(S_R_P_LAST_VESSEL, StringUtility.toUpperCase(lastPre.getVesselName()));
                dict.put(S_R_P_LAST_VOYAGE, StringUtility.toUpperCase(lastPre.getVoyage()));
            }

            // Add first pre routing info
            if (firstPre != null) {
                dict.put(S_R_P_FIRST_VESSEL, StringUtility.toUpperCase(firstPre.getVesselName()));
                dict.put(S_R_P_FIRST_VOYAGE, StringUtility.toUpperCase(firstPre.getVoyage()));
            }
        }
    }

    // Adds each party's mapped data using their type (like SHIPPER, CONSIGNEE) as the key
    private void addShipmentPartyDetails(Map<String, Object> dict, List<Parties> parties) {
        if (parties == null) {
            return;
        }

        for (Parties party : parties) {
            if (party != null && party.getType() != null) {
                dict.put("S_" + party.getType().replaceAll("\\s+", ""), buildPartyMap(party));
                dict.put("S_" + party.getType().replaceAll("\\s+", "") + CONTACT, buildPartyContact(party));
            }
        }
    }

    // Adds single agent party (either origin or destination) using a provided key
    private void addShipmentAgentDetails(Map<String, Object> dict, String key, Parties agent) {
        if (agent != null) {
            dict.put(key, buildPartyMap(agent));
            dict.put(key + CONTACT, buildPartyContact(agent));
        }
    }

    // Adds origin/receiving branches and triangulated partner branch info
    private void addShipmentBranchAndTriangulationDetails(Map<String, Object> dict, ShipmentDetails details) {
        Long origin = details.getOriginBranch();
        Long receiving = details.getReceivingBranch();
        List<TriangulationPartner> triangulations = details.getTriangulationPartnerList();

        populateBranchAndTriangulationInfo(dict, origin, receiving, triangulations, "S" );
    }

    private void populateBranchAndTriangulationInfo(Map<String, Object> dict, Long origin, Long receiving, List<TriangulationPartner> triangulations, String prefix) {

        if (origin == null || receiving == null) {
            return; // cannot proceed if key branches are missing
        }

        Set<String> tenantIds = new HashSet<>();
        tenantIds.add(origin.toString());
        tenantIds.add(receiving.toString());

        // Add triangulation partner tenant IDs if present
        if (triangulations != null) {
            tenantIds.addAll(triangulations.stream()
                    .filter(Objects::nonNull)
                    .map(tp -> tp.getTriangulationPartner().toString())
                    .collect(Collectors.toSet()));
        }

        // Fetch full tenant data in bulk
        Map<String, TenantModel> tenantData = masterDataUtils.fetchInTenantsList(tenantIds);
        masterDataUtils.pushToCache(tenantData, CacheConstants.TENANTS, tenantIds, new TenantModel(), null);

        // Add origin & destination branches
        dict.put(prefix +"_OriginBranch", buildTenantMap(tenantData.get(origin.toString())));
        dict.put(prefix +"_OriginBranchContact", buildTenantContact(tenantData.get(origin.toString())));
        dict.put(prefix +"_DestinationBranch", buildTenantMap(tenantData.get(receiving.toString())));
        dict.put(prefix +"_DestinationBranchContact", buildTenantContact(tenantData.get(receiving.toString())));

        // Add triangulation partner branches with indexed keys
        if (triangulations != null) {
            for (int i = 0; i < triangulations.size(); i++) {
                TriangulationPartner tp = triangulations.get(i);
                if (tp != null && tp.getTriangulationPartner() != null) {
                    TenantModel model = tenantData.get(tp.getTriangulationPartner().toString());
                    dict.put(prefix +"_TriangulationBranch" + (i + 1), buildTenantMap(model));
                    dict.put(prefix +"_TriangulationBranch" + (i + 1) + CONTACT, buildTenantContact(model));
                }
            }
        }
    }
}
