package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;

import javax.persistence.*;
import java.time.LocalDateTime;


@Entity
@Setter
@Getter
@Table(name = "arrival_departure_details")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@BatchSize(size = 50)
public class ArrivalDepartureDetails extends MultiTenancy {

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "container_yard_id", referencedColumnName = "id")
    @OrganizationData
    private Parties containerYardId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "transport_port_id", referencedColumnName = "id")
    @OrganizationData
    private Parties transportPortId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "cto_id", referencedColumnName = "id")
    @OrganizationData
    private Parties CTOId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "cfs_id", referencedColumnName = "id")
    @OrganizationData
    private Parties CFSId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "first_foreign_port_id", referencedColumnName = "id")
    @OrganizationData
    private Parties firstForeignPortId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "last_foreign_port_id", referencedColumnName = "id")
    @OrganizationData
    private Parties lastForeignPortId;

    @Column(name = "first_foreign_port")
    @UnlocationData
    private String firstForeignPort;

    @Column(name = "last_foreign_port")
    @UnlocationData
    private String lastForeignPort;

    @Column(name = "type")
    private String type;

    @Column(name = "first_foreign_port_arrival_date")
    private LocalDateTime firstForeignPortArrivalDate;

    @Column(name = "last_foreign_port_departure_date")
    private LocalDateTime lastForeignPortDepartureDate;

}
