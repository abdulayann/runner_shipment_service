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
@Table(name = "hbl_lock_settings")
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE hbl_lock_settings SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class HblLockSettings extends MultiTenancy {

    @Column(name = "consignor_name_lock")
    private Boolean consignorNameLock;

    @Column(name = "consignor_address_lock")
    private Boolean consignorAddressLock;

    @Column(name = "consignee_name_lock")
    private Boolean consigneeNameLock;

    @Column(name = "consignee_address_lock")
    private Boolean consigneeAddressLock;

    @Column(name = "notify_party_name_lock")
    private Boolean notifyPartyNameLock;

    @Column(name = "notify_party_address_lock")
    private Boolean notifyPartyAddressLock;

    @Column(name = "notify_party_tax_id_lock")
    private Boolean notifyPartyTaxIdLock;

    @Column(name = "notify_party_email_lock")
    private Boolean notifyPartyEmailLock;

    @Column(name = "additional_notify_party_name_lock")
    private Boolean additionalNotifyPartyNameLock;

    @Column(name = "additional_notify_party_address_lock")
    private Boolean additionalNotifyPartyAddressLock;

    @Column(name = "additional_notify_party_tax_id_lock")
    private Boolean additionalNotifyPartyTaxIdLock;

    @Column(name = "additional_notify_party_email_lock")
    private Boolean additionalNotifyPartyEmailLock;

    @Column(name = "additional_notify_party_name2_lock")
    private Boolean additionalNotifyPartyName2Lock;

    @Column(name = "additional_notify_party_address2_lock")
    private Boolean additionalNotifyPartyAddress2Lock;

    @Column(name = "additional_notify_party_tax_id2_lock")
    private Boolean additionalNotifyPartyTaxId2Lock;

    @Column(name = "additional_notify_party_email2_lock")
    private Boolean additionalNotifyPartyEmail2Lock;

    @Column(name = "purchase_order_number_lock")
    private Boolean purchaseOrderNumberLock;

    @Column(name = "exporter_reference_number_lock")
    private Boolean exporterReferenceNumberLock;

    @Column(name = "bl_reference_number_lock")
    private Boolean blReferenceNumberLock;

    @Column(name = "origin_of_goods_lock")
    private Boolean originOfGoodsLock;

    @Column(name = "place_of_receipt_lock")
    private Boolean placeOfReceiptLock;

    @Column(name = "port_of_load_lock")
    private Boolean portOfLoadLock;

    @Column(name = "port_of_discharge_lock")
    private Boolean portOfDischargeLock;

    @Column(name = "place_of_delivery_lock")
    private Boolean placeOfDeliveryLock;

    @Column(name = "package_count_lock")
    private Boolean packageCountLock;

    @Column(name = "package_type_lock")
    private Boolean packageTypeLock;

    @Column(name = "hs_code_lock")
    private Boolean hsCodeLock;

    @Column(name = "schedule_b_number_lock")
    private Boolean scheduleBNumberLock;

    @Column(name = "cargo_description_lock")
    private Boolean cargoDescriptionLock;

    @Column(name = "marks_and_numbers_lock")
    private Boolean marksAndNumbersLock;

    @Column(name = "hazmat_details_lock")
    private Boolean hazmatDetailsLock;

    @Column(name = "cargo_gross_weight_lock")
    private Boolean cargoGrossWeightLock;

    @Column(name = "cargo_net_weight_lock")
    private Boolean cargoNetWeightLock;

    @Column(name = "cargo_gross_volume_lock")
    private Boolean cargoGrossVolumeLock;

    @Column(name = "shipper_declared_value_lock")
    private Boolean shipperDeclaredValueLock;

    @Column(name = "shipper_declared_currency_lock")
    private Boolean shipperDeclaredCurrencyLock;

    @Column(name = "bl_instruction_type_lock")
    private Boolean blInstructionTypeLock;

    @Column(name = "bl_comments_lock")
    private Boolean blCommentsLock;

    @Column(name = "consignor_tax_id_lock")
    private Boolean consignorTaxIdLock;

    @Column(name = "consignor_reference_number_lock")
    private Boolean consignorReferenceNumberLock;

    @Column(name = "consignee_tax_id_lock")
    private Boolean consigneeTaxIdLock;

    @Column(name = "consignee_reference_number_lock")
    private Boolean consigneeReferenceNumberLock;

    @Column(name = "number_of_copies_lock")
    private Boolean numberOfCopiesLock;

    @Column(name = "gross_weight_unit_lock")
    private Boolean grossWeightUnitLock;

    @Column(name = "weight_unit_lock")
    private Boolean weightUnitLock;

    @Column(name = "volume_unit_lock")
    private Boolean volumeUnitLock;

    @Column(name = "container_type_lock")
    private Boolean containerTypeLock;

    @Column(name = "carrier_seal_number_lock")
    private Boolean carrierSealNumberLock;

    @Column(name = "shipper_seal_number_lock")
    private Boolean shipperSealNumberLock;

    @Column(name = "container_gross_weight_lock")
    private Boolean containerGrossWeightLock;

    @Column(name = "container_gross_weight_unit_lock")
    private Boolean containerGrossWeightUnitLock;

    @Column(name = "container_gross_volume_lock")
    private Boolean containerGrossVolumeLock;

    @Column(name = "container_gross_volume_unit_lock")
    private Boolean containerGrossVolumeUnitLock;

    @Column(name = "container_number_lock")
    private Boolean containerNumberLock;

    @Column(name = "bl_container_id_lock")
    private Boolean blContainerIdLock;

    @Column(name = "cargo_gross_weight_unit_lock")
    private Boolean cargoGrossWeightUnitLock;

    @Column(name = "cargo_net_weight_unit_lock")
    private Boolean cargoNetWeightUnitLock;

    @Column(name = "cargo_gross_volume_unit_lock")
    private Boolean cargoGrossVolumeUnitLock;

    @Column(name = "classification_lock")
    private Boolean classificationLock;

    @Column(name = "un_no_lock")
    private Boolean unNoLock;

    @Column(name = "imco_no_lock")
    private Boolean imcoNoLock;

    @Column(name = "container_desc_lock")
    private Boolean containerDescLock;

    @Column(name = "vessel_name_lock")
    private Boolean vesselNameLock;

    @Column(name = "voyage_lock")
    private Boolean voyageLock;

    @Column(name = "house_bill_lock")
    private Boolean houseBillLock;

    @Column(name = "transport_type_lock")
    private Boolean transportTypeLock;

    @Column(name = "shipment_type_lock")
    private Boolean shipmentTypeLock;

    @Column(name = "etd")
    private Boolean etd;

    @Column(name = "shipping_time")
    private Boolean shippingTime;

    @Column(name = "inco_terms")
    private Boolean incoTerms;

    @Column(name = "inco_term_place")
    private Boolean incoTermPlace;

    @Column(name = "payment_term")
    private Boolean paymentTerm;

    @Column(name = "final_destination")
    private Boolean finalDestination;

    @Column(name = "el_number")
    private Boolean elNumber;

    @Column(name = "lc_number")
    private Boolean lcNumber;

    @Column(name = "el_date")
    private Boolean elDate;

    @Column(name = "invoice_numbers")
    private Boolean invoiceNumbers;

    @Column(name = "quantity")
    private Boolean quantity;

    @Column(name = "quantity_code")
    private Boolean quantityCode;

    @Column(name = "bl_delivery_agent")
    private Boolean blDeliveryAgent;

    @Column(name = "bl_delivery_agent_address")
    private Boolean blDeliveryAgentAddress;

    @Column(name = "bl_terms_and_conditions_id_lock")
    private Boolean bLTermsandConditionsIdLock;

    @Column(name = "cargo_terms_lock")
    private Boolean cargoTermsLock;

    @Column(name = "cargo_terms_description_lock")
    private Boolean cargoTermsDescriptionLock;

    @Column(name = "bl_remarks_lock")
    private Boolean bLRemarksLock;

    @Column(name = "bl_remarks_description_lock")
    private Boolean bLRemarksDescriptionLock;
}
