package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;


@Entity
@Data
@Table(name = "customer_booking")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE customer_booking SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class CustomerBooking extends MultiTenancy {

    @Enumerated(EnumType.STRING)
    @Column(name = "booking_status")
    private BookingStatus bookingStatus;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "customer_id", referencedColumnName = "id")
    private Parties customer;

    @Column(name = "is_customer_free_text")
    private Boolean isCustomerFreeText;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consignor_id", referencedColumnName = "id")
    private Parties consignor;

    @Column(name = "is_consignor_free_text")
    private Boolean isConsignorFreeText;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consignee_id", referencedColumnName = "id")
    private Parties consignee;

    @Column(name = "is_consignee_free_text")
    private Boolean isConsigneeFreeText;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "notify_party_id", referencedColumnName = "id")
    private Parties notifyParty;

    @Column(name = "is_notify_party_free_text")
    private Boolean isNotifyPartyFreeText;

    @Column(name = "customer_email")
    private String customerEmail;

    @Column(name = "booking_number")
    private String bookingNumber;

    @Column(name = "booking_date")
    private LocalDateTime bookingDate;

    @Column(name = "inco_terms")
    private String incoTerms;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = CarrierDetails.class)
    @JoinColumn(name = "carrier_detail_id", referencedColumnName = "id")
    private CarrierDetails carrierDetails;

    @Column(name = "transport_type")
    private String transportType;

    @Column(name = "cargo_type")
    private String cargoType;

    @Column(name = "direction")
    private String direction;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "quantity_unit")
    private String quantityUnit;

    @Column(name = "gross_weight")
    private BigDecimal grossWeight;

    @Column(name = "gross_weight_unit")
    private String grossWeightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    private String volumeUnit;

    @Column(name = "weight_volume")
    private BigDecimal weightVolume;

    @Column(name = "weight_volume_unit")
    private String weightVolumeUnit;

    @Column(name = "chargeable")
    private BigDecimal chargeable;

    @Column(name = "chargeable_unit")
    private String chargeableUnit;

    @Column(name = "contract_id")
    private UUID contractId;

    @OneToMany(fetch = FetchType.LAZY, mappedBy =  "bookingId")
    private List<Containers> containersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "bookingId")
    private List<Packing> packingList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "bookingId")
    private List<Routings> routingList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "bookingId")
    private List<BookingCharges> bookingCharges;

}
