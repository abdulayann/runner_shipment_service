package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.SIStatus;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;
import java.util.List;


@Entity
@Data
@Table(name = "shipping_instruction")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE shipping_instruction SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class ShippingInstruction extends MultiTenancy {

    @Column(name = "shipment_guid")
    public UUID shipmentGuid;

    @Column(name="consolidation_guid")
    private UUID consolidationGuid;

    @Column(name = "booking_id")
    private Long bookingId;

    @Column(name = "is_uca")
    private Boolean isUca;

    @OneToOne(targetEntity = CarrierDetails.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "carrier_detail_id", referencedColumnName = "id")
    private CarrierDetails carrierDetails;

    @Column(name = "carrier")
    @Size(max = 255, message = "max size is 255 for carrier")
    private String carrier;

    @Column(name = "carrier_booking_number")
    private String carrierBookingNumber;

    @Column(name = "email")
    @Size(max = 255, message = "max size is 255 for email")
    private String email;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "shipper_id", referencedColumnName = "id")
    @OrganizationData
    private Parties shipper;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "forwarder_id", referencedColumnName = "id")
    @OrganizationData
    private Parties forwarder;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consignee_id", referencedColumnName = "id")
    @OrganizationData
    private Parties consignee;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "notify_party_id", referencedColumnName = "id")
    private Parties notifyParty;

    @Column(name = "origin_of_goods")
    private String originOfGoods;

    @Column(name = "place_of_carrier_receipt")
    @UnlocationData
    private String placeOfCarrierReceipt;

    @Column(name = "place_of_carrier_delivery")
    @UnlocationData
    private String placeOfCarrierDelivery;

    @Column(name = "imo_number")
    @Size(max = 255, message = "max size is 255 for imo_number")
    private String imoNumber;

    @Column(name = "service_type")
    @Size(max=3, message = "max size is 3 for service Type")
    @MasterData(type = MasterDataType.SERVICE_MODE)
    private String serviceType;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "ship_to_id", referencedColumnName = "id")
    private Parties shipTo;

    @Column(name = "shipment_type")
    @MasterData(type = MasterDataType.CUSTOM_SHIPMENT_TYPE)
    private String shipmentType;

    @Column(name = "total_container_count")
    @Size(max = 255, message = "max size is 255 for total_container_count")
    private String totalContainerCount;

    @Column(name = "total_package_count")
    private String totalPackageCount;

    @Column(name = "total_shipment_weight")
    private String totalShipmentWeight;

    @Column(name = "total_shipment_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String totalShipmentWeightUnit;

    @Column(name = "total_shipment_volume")
    private BigDecimal totalShipmentVolume;

    @Column(name = "total_shipment_volume_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String totalShipmentVolumeUnit;

//    @Column(name = "chec")
//    private String shipperTaxId; // check
//
//    @Column(name = "chec")
//    private String consigneeTaxId; // check
//
//    @Column(name = "chec")
//    private String notifyPartyTaxId; // check

    @Column(name = "hbl_filling_info")
    @MasterData(type = MasterDataType.HBL_FILING_INFO)
    private String hblFillingInfo;

    @Column(name = "pcin")
    private String pcin;

    @Column(name = "csn")
    private String csn;

    @Column(name = "mcin")
    private String mcin;

    @Column(name = "shipper_declared_value")
    private String shipperDeclaredValue;

    @Column(name = "shipper_declared_value_currency")
    private String shipperDeclaredValueCurrency;

    @Column(name = "user_defined_clauses")
    private String userDefinedClauses;

    @Column(name = "bl_release_office")
    @UnlocationData
    private String blReleaseOffice;

    @Column(name = "req_date_of_issue")
    private LocalDateTime reqDateOfIssue;

    @Column(name = "original_seaway")
    private Boolean originalSeaway;

    @Column(name = "is_stand_alone_hbl")
    private Boolean isStandAloneHbl;

    @Column(name = "hbl_number")
    private Boolean hblNumber;

    @Column(name = "original_bill")
    private Boolean originalBill;

    @Column(name = "original_freighted_copies")
    private String originalFreightedCopies;

    @Column(name = "original_un_freighted_copies")
    private String originalUnFreightedCopies;

    @Column(name = "original_non_negotiable_freighted_copies")
    private String originalNonNegotiableFreightedCopies;

    @Column(name = "original_non_negotiable_un_freighted_copies")
    private String originalNonNegotiableUnFreightedCopies;

    @Column(name = "seaway_express_bill")
    private Boolean seawayExpressBill;

    @Column(name = "seaway_express_freighted_copies")
    @Size(max=255, message = "max size is 255 for seaway_express_freighted_copies")
    private String seawayExpressFreightedCopies;

    @Column(name = "seaway_express_un_freighted_copies")
    @Size(max=255, message = "max size is 255 for seaway_express_un_freighted_copies")
    private String seawayExpressUnFreightedCopies;

    @Column(name = "bl_comments")
    private String blComments;

    @OneToMany(fetch = FetchType.LAZY, mappedBy =  "shippingInstructionId")
    @BatchSize(size = 50)
    private List<Containers> containersList;

    @Enumerated(EnumType.STRING)
    @Column(name = "status")
    private SIStatus status;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shippingInstructionId")
    @BatchSize(size = 50)
    private List<Packing> packingList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "event_code = 'CarrierBookingEvents'")
    @BatchSize(size = 50)
    private List<Events> eventsList; // todo:

    @Column(name = "b_l_object_status")
    private String bLObjectStatus;

    @Column(name = "consignor_name")
    private String consignorName;

    @Column(name = "consignor_address")
    private String consignorAddress;

    @Column(name = "consignor_tax_id")
    private String consignorTaxId;

    @Column(name = "consignor_reference_number")
    private String consignorReferenceNumber;

    @Column(name = "consignee_name")
    private String consigneeName;

    @Column(name = "consignee_address")
    private String consigneeAddress;

    @Column(name = "consignee_reference_number")
    private String consigneeReferenceNumber;

    @Column(name = "purchase_order_number")
    private String purchaseOrderNumber;

    @Column(name = "exporter_reference_number")
    private String exporterReferenceNumber;

    @Column(name = "b_l_reference_number")
    private String bLReferenceNumber;

    @Column(name = "place_of_receipt")
    private String placeOfReceipt;

    @Column(name = "port_of_load")
    private String portOfLoad;

    @Column(name = "port_of_discharge")
    private String portOfDischarge;

    @Column(name = "place_of_delivery")
    private String placeOfDelivery;

    @Column(name = "no_of_copies")
    private String noOfCopies;

    @Column(name = "reason")
    private String reason;

    @Column(name = "is_sync_with_bl_object")
    private Boolean isSyncWithBlObject;

//        [ServiceLookupSelectionEditor(typeof(Entities.ClausesRow), Multiple = true, SelectionDialogColumn = "Default.Clauses", Picker = true)]
//            [DisplayName("Clauses")]
//            [LinkingSetRelation(typeof(ShippingInstrauctionClauseLinkRow), "ShippingInstructionId", "ClauseId")]
//    public List<Int64> Clauses
//    {
//        get { return Fields.Clauses[this]; }
//        set { Fields.Clauses[this] = value; }
//    }

//        [NotMapped]
//    public List<BlObjectCargosRow> BlObjectCorgoes
//    {
//        get { return Fields.BlObjectCorgoes[this]; }
//        set { Fields.BlObjectCorgoes[this] = value; }
//    }
//        [NotMapped]
//    public List<BlObjectContainersRow> BlObjectContainers
//    {
//        get { return Fields.BlObjectContainers[this]; }
//        set { Fields.BlObjectContainers[this] = value; }
//    }
//
//        [NotMapped]
//    public List<BlObjectNotifyPartyRow> BlObjectNotifyParty
//    {
//        get { return Fields.BlObjectNotifyParty[this]; }
//        set { Fields.BlObjectNotifyParty[this] = value; }
//    }
}
