package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.apache.poi.hpsf.Decimal;
import org.hibernate.annotations.Generated;
import org.hibernate.annotations.*;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.UUID;


@Entity
@Setter
@Getter
@Table(name = "arrival_departure_details")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
public class ArrivalDepartureDetails extends MultiTenancy {

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "container_yard_id", referencedColumnName = "id")
    private Parties containerYardId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "transport_port_id", referencedColumnName = "id")
    private Parties transportPortId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "cto_id", referencedColumnName = "id")
    private Parties CTOId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "cfs_id", referencedColumnName = "id")
    private Parties CFSId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "first_foreign_port_id", referencedColumnName = "id")
    private Parties firstForeignPortId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "last_foreign_port_id", referencedColumnName = "id")
    private Parties lastForeignPortId;

    @Column(name = "first_foreign_port")
    private String firstForeignPort;

    @Column(name = "last_foreign_port")
    private String lastForeignPort;

    @Column(name = "type")
    private String type;

    @Column(name = "first_foreign_port_arrival_date")
    private LocalDateTime firstForeignPortArrivalDate;

    @Column(name = "last_foreign_port_departure_date")
    private LocalDateTime lastForeignPortDepartureDate;

}
