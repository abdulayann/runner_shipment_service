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
    @JoinColumn(name = "departure_container_yard_id", referencedColumnName = "id")
    private Parties dContainerYardId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "departure_transport_port_id", referencedColumnName = "id")
    private Parties dTransportPortId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "departure_first_foreign_port_id", referencedColumnName = "id")
    private Parties dFirstForeignPortId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "departure_last_foreign_port_id", referencedColumnName = "id")
    private Parties dLastForeignPortId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "arrival_container_yard_id", referencedColumnName = "id")
    private Parties aContainerYardId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "arrival_transport_port_id", referencedColumnName = "id")
    private Parties aTransportPortId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "arrival_first_arrival_port_id", referencedColumnName = "id")
    private Parties aFirstArrivalPortId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "arrival_last_foreign_port_id", referencedColumnName = "id")
    private Parties aLastForeignPortId;

    @Column(name = "departure_first_foreign_port_arrival_date")
    private LocalDateTime dFirstForeignPortArrivalDate;

    @Column(name = "departure_last_foreign_port_departure_date")
    private LocalDateTime dLastForeignPortDepartureDate;

    @Column(name = "arrival_first_arrival_port_arrival_date")
    private LocalDateTime aFirstArrivalPortArrivalDate;

    @Column(name = "arrival_last_foreign_port_departure_date")
    private LocalDateTime aLastForeignPortDepartureDate;


}
