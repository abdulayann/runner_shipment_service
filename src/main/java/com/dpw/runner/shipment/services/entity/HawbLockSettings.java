package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Getter
@Setter
@Table(name = "hawb_lock_settings")
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE hawb_lock_settings SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class HawbLockSettings extends MultiTenancy {

    @Column(name = "shipper_name_lock")
    private Boolean shipperNameLock;

    @Column(name = "shipper_address_lock")
    private Boolean shipperAddressLock;

    @Column(name = "consignee_name_lock")
    private Boolean consigneeNameLock;

    @Column(name = "consignee_address_lock")
    private Boolean consigneeAddressLock;

    @Column(name = "issuing_agent_name_lock")
    private Boolean issuingAgentNameLock;

    @Column(name = "issuing_agent_address_lock")
    private Boolean issuingAgentAddressLock;

    @Column(name = "iata_code_lock")
    private Boolean iataCodeLock;

    @Column(name = "awb_number_lock")
    private Boolean awbNumberLock;

    @Column(name = "notify_party_lock")
    private Boolean notifyPartyLock;

    @Column(name = "routing_details_lock")
    private Boolean routingDetailsLock;

    @Column(name = "accounting_info_lock")
    private Boolean accountingInfoLock;

    @Column(name = "handling_info_lock")
    private Boolean handlingInfoLock;

    @Column(name = "other_info_lock")
    private Boolean otherInfoLock;

    @Column(name = "ntr_qty_goods_lock")
    private Boolean ntrQtyGoodsLock;

    @Column(name = "special_handling_codes_lock")
    private Boolean specialHandlingCodesLock;

    @Column(name = "reference_number_lock")
    private Boolean referenceNumberLock;

    @Column(name = "shipping_information_lock")
    private Boolean shippingInformationLock;

    @Column(name = "sci_lock")
    private Boolean sciLock;

    @Column(name = "currency_lock")
    private Boolean currencyLock;

    @Column(name = "charge_code_lock")
    private Boolean chargeCodeLock;

    @Column(name = "carriage_value_lock")
    private Boolean carriageValueLock;

    @Column(name = "customs_value_lock")
    private Boolean customsValueLock;

    @Column(name = "insurance_amount_lock")
    private Boolean insuranceAmountLock;

    @Column(name = "custom_origin_code_lock")
    private Boolean customOriginCodeLock;

    @Column(name = "goods_description_lock")
    private Boolean goodsDescriptionLock;

    @Column(name = "packing_lock")
    private Boolean packingLock;

    @Column(name = "prepaid_due_agent_charges_lock")
    private Boolean prepaidDueAgentChargesLock;

    @Column(name = "prepaid_due_carrier_charges_lock")
    private Boolean prepaidDueCarrierChargesLock;

    @Column(name = "total_prepaid_lock")
    private Boolean totalPrepaidLock;

    @Column(name = "collect_weight_charges_lock")
    private Boolean collectWeightChargesLock;

    @Column(name = "collect_valuation_charge_lock")
    private Boolean collectValuationChargeLock;

    @Column(name = "collect_tax_lock")
    private Boolean collectTaxLock;

    @Column(name = "total_collect_lock")
    private Boolean totalCollectLock;

    @Column(name = "other_charges_lock")
    private Boolean otherChargesLock;

    @Column(name = "shipper_lock")
    private Boolean shipperLock;

    @Column(name = "carrier_lock")
    private Boolean carrierLock;

    @Column(name = "executed_at_lock")
    private Boolean executedAtLock;

    @Column(name = "executed_on_lock")
    private Boolean executedOnLock;

    @Column(name = "other_customs_lock")
    private Boolean otherCustomsLock;

    @Column(name = "notify_organization_lock")
    private Boolean notifyOrganizationLock;

    @Column(name = "notify_organization_address_lock")
    private Boolean notifyOrganizationAddressLock;

    @Column(name = "airport_of_departure_lock")
    private Boolean airportOfDepartureLock;

    @Column(name = "airport_of_destination_lock")
    private Boolean airportOfDestinationLock;

    @Column(name = "first_carrier_lock")
    private Boolean firstCarrierLock;

    @Column(name = "origin_port_lock")
    private Boolean originPortLock;

    @Column(name = "destination_port_lock")
    private Boolean destinationPortLock;

    @Column(name = "by_carrier_lock")
    private Boolean byCarrierLock;

    @Column(name = "flight_number_lock")
    private Boolean flightNumberLock;

    @Column(name = "flight_date_lock")
    private Boolean flightDateLock;

    @Column(name = "information_identifier_lock")
    private Boolean informationIdentifierLock;

    @Column(name = "trade_identification_code_lock")
    private Boolean tradeIdentificationCodeLock;

    @Column(name = "trade_identification_comment_lock")
    private Boolean tradeIdentificationCommentLock;

    @Column(name = "charge_description_lock")
    private Boolean chargeDescriptionLock;

    @Column(name = "rate_lock")
    private Boolean rateLock;

    @Column(name = "charge_basis_lock")
    private Boolean chargeBasisLock;

    @Column(name = "charges_due_lock")
    private Boolean chargesDueLock;

    @Column(name = "pieces_no_lock")
    private Boolean piecesNoLock;

    @Column(name = "gross_wt_lock")
    private Boolean grossWtLock;

    @Column(name = "gross_wt_unit_lock")
    private Boolean grossWtUnitLock;

    @Column(name = "rate_class_lock")
    private Boolean rateClassLock;

    @Column(name = "commodity_item_no_lock")
    private Boolean commodityItemNoLock;

    @Column(name = "chargeable_wt_lock")
    private Boolean chargeableWtLock;

    @Column(name = "rate_charge_lock")
    private Boolean rateChargeLock;

    @Column(name = "total_amount_lock")
    private Boolean totalAmountLock;

    @Column(name = "slac_code_lock")
    private Boolean slacCodeLock;

    @Column(name = "hs_code_lock")
    private Boolean hsCodeLock;

    @Column(name = "packing_awb_goods_desc_id_lock")
    private Boolean packingAwbGoodsDescIdLock;

    @Column(name = "packing_packs_lock")
    private Boolean packingPacksLock;

    @Column(name = "packing_packs_type_lock")
    private Boolean packingPacksTypeLock;

    @Column(name = "packing_origin_lock")
    private Boolean packingOriginLock;

    @Column(name = "packing_order_lock")
    private Boolean packingOrderLock;

    @Column(name = "packing_length_lock")
    private Boolean packingLengthLock;

    @Column(name = "packing_length_unit_lock")
    private Boolean packingLengthUnitLock;

    @Column(name = "packing_width_lock")
    private Boolean packingWidthLock;

    @Column(name = "packing_width_unit_lock")
    private Boolean packingWidthUnitLock;

    @Column(name = "packing_height_lock")
    private Boolean packingHeightLock;

    @Column(name = "packing_height_unit_lock")
    private Boolean packingHeightUnitLock;

    @Column(name = "packing_weight_lock")
    private Boolean packingWeightLock;

    @Column(name = "packing_weight_unit_lock")
    private Boolean packingWeightUnitLock;

    @Column(name = "packing_volume_lock")
    private Boolean packingVolumeLock;

    @Column(name = "packing_volume_unit_lock")
    private Boolean packingVolumeUnitLock;

    @Column(name = "packing_net_weight_lock")
    private Boolean packingNetWeightLock;

    @Column(name = "packing_net_weight_unit_lock")
    private Boolean packingNetWeightUnitLock;

    @Column(name = "packing_volume_weight_lock")
    private Boolean packingVolumeWeightLock;

    @Column(name = "packing_volume_weight_unit_lock")
    private Boolean packingVolumeWeightUnitLock;

    @Column(name = "packing_marksn_nums_lock")
    private Boolean packingMarksnNumsLock;

    @Column(name = "packing_country_code_lock")
    private Boolean packingCountryCodeLock;

    @Column(name = "packing_goods_desc_lock")
    private Boolean packingGoodsDescLock;

    @Column(name = "packing_reference_number_lock")
    private Boolean packingReferenceNumberLock;

    @Column(name = "packing_inspections_lock")
    private Boolean packingInspectionsLock;

    @Column(name = "packing_dg_class_lock")
    private Boolean packingDgClassLock;

    @Column(name = "packing_dg_substance_id_lock")
    private Boolean packingDgSubstanceIdLock;

    @Column(name = "packing_min_temp_lock")
    private Boolean packingMinTempLock;

    @Column(name = "packing_min_temp_unit_lock")
    private Boolean packingMinTempUnitLock;

    @Column(name = "packing_max_temp_lock")
    private Boolean packingMaxTempLock;

    @Column(name = "packing_max_temp_unit_lock")
    private Boolean packingMaxTempUnitLock;

    @Column(name = "packing_commodity_lock")
    private Boolean packingCommodityLock;

    @Column(name = "packing_hs_code_lock")
    private Boolean packingHsCodeLock;

    @Column(name = "shipper_address_line2_lock")
    private Boolean shipperAddressLine2Lock;

    @Column(name = "shipper_city_lock")
    private Boolean shipperCityLock;

    @Column(name = "shipper_state_code")
    private Boolean shipperStateCode;

    @Column(name = "shipper_zip_code")
    private Boolean shipperZipCode;

    @Column(name = "shipper_country_code_lock")
    private Boolean shipperCountyCodeLock;

    @Column(name = "shipper_country_name_lock")
    private Boolean shipperCountryNameLock;

    @Column(name = "shipper_contact_name_lock")
    private Boolean shipperContactNameLock;

    @Column(name = "shipper_phone_number_lock")
    private Boolean shipperPhoneNumberLock;

    @Column(name = "shipper_tax_number_lock")
    private Boolean shipperTAXNumberLock;

    @Column(name = "shipper_account_number")
    private Boolean shipperAccountNumber;

    @Column(name = "consignee_address_line2_lock")
    private Boolean consigneeAddressLine2Lock;

    @Column(name = "consignee_city_lock")
    private Boolean consigneeCityLock;

    @Column(name = "consignee_state_code")
    private Boolean consigneeStateCode;

    @Column(name = "consignee_zip_code")
    private Boolean consigneeZipCode;

    @Column(name = "consignee_country_code_lock")
    private Boolean consigneeCountyCodeLock;

    @Column(name = "consignee_country_name_lock")
    private Boolean consigneeCountryNameLock;

    @Column(name = "consignee_contact_name_lock")
    private Boolean consigneeContactNameLock;

    @Column(name = "consignee_phone_number_lock")
    private Boolean consigneePhoneNumberLock;

    @Column(name = "consignee_tax_number_lock")
    private Boolean consigneeTAXNumberLock;

    @Column(name = "consignee_account_number")
    private Boolean consigneeAccountNumber;

    @Column(name = "notify_address_line2_lock")
    private Boolean notifyAddressLine2Lock;

    @Column(name = "notify_city_lock")
    private Boolean notifyCityLock;

    @Column(name = "notify_state_code")
    private Boolean notifyStateCode;

    @Column(name = "notify_zip_code")
    private Boolean notifyZipCode;

    @Column(name = "notify_country_code_lock")
    private Boolean notifyCountyCodeLock;

    @Column(name = "notify_country_name_lock")
    private Boolean notifyCountryNameLock;

    @Column(name = "notify_contact_name_lock")
    private Boolean notifyContactNameLock;

    @Column(name = "notify_phone_number_lock")
    private Boolean notifyPhoneNumberLock;

    @Column(name = "notify_tax_number_lock")
    private Boolean notifyTAXNumberLock;

    @Column(name = "issuing_agent_address_line2_lock")
    private Boolean issuingAgentAddressLine2Lock;

    @Column(name = "issuing_agent_city_lock")
    private Boolean issuingAgentCityLock;

    @Column(name = "issuing_agent_state_code")
    private Boolean issuingAgentStateCode;

    @Column(name = "issuing_agent_zip_code")
    private Boolean issuingAgentZipCode;

    @Column(name = "issuing_agent_country_code_lock")
    private Boolean issuingAgentCountyCodeLock;

    @Column(name = "issuing_agent_country_name_lock")
    private Boolean issuingAgentCountryNameLock;

    @Column(name = "issuing_agent_contact_name_lock")
    private Boolean issuingAgentContactNameLock;

    @Column(name = "issuing_agent_phone_number_lock")
    private Boolean issuingAgentPhoneNumberLock;

    @Column(name = "issuing_agent_tax_number_lock")
    private Boolean issuingAgentTAXNumberLock;

    @Column(name = "specified_address_location")
    private Boolean specifiedAddressLocation;

    @Column(name = "cass_code")
    private Boolean cassCode;

    @Column(name = "account_number")
    private Boolean accountNumber;

    @Column(name = "other_information_code_lock")
    private Boolean otherInformationCodeLock;

    @Column(name = "override_auto_update_lock")
    private Boolean overrideAutoUpdateLock;

    @Column(name = "country_code_lock")
    private Boolean countryCodeLock;

    @Column(name = "ra_number_lock")
    private Boolean raNumberLock;

    @Column(name = "ra_expiry_date_lock")
    private Boolean raExpiryDateLock;

    @Column(name = "screening_status_lock")
    private Boolean screeningStatusLock;

    @Column(name = "screening_time_lock")
    private Boolean screeningTimeLock;

    @Column(name = "screener_name_lock")
    private Boolean screenerNameLock;

    @Column(name = "gross_volume_lock")
    private Boolean grossVolumeLock;

    @Column(name = "gross_volume_unit_lock")
    private Boolean grossVolumeUnitLock;

    @Column(name = "dimensions_lock")
    private Boolean dimensionsLock;

    @Column(name = "version_lock")
    private Boolean versionLock;
}
