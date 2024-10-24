package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "mawb_stocks")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@SQLDelete(sql = "UPDATE mawb_stocks SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class MawbStocks extends MultiTenancy {
    @Column(name = "consolidation_id")
    private Long consolidationId;

    @Column(name = "mawb_number")
    private String mawbNumber;

    @Column(name = "next_mawb_number")
    private String nextMawbNumber;

    @Column(name = "available_count")
    private String availableCount;

    @Column(name = "status")
    private String status;

    @Column(name = "home_port")
    private String homePort;

    @Column(name = "air_line_prefix")
    private String airLinePrefix;

    @Column(name = "prefix")
    private String prefix;

    @Column(name = "seq_number")
    private String seqNumber;

    @Column(name = "count")
    private String count;

    @Column(name = "start_number")
    private Long startNumber;

    @Column(name = "mawb_stocks_from")
    private String from;

    @Column(name = "mawb_stocks_to")
    private String to;

    @Column(name = "borrowed_from")
    private String borrowedFrom;

    @Column(name = "borrowed_from_full_name")
    private String borrowedFromFullName;

    @OneToMany(mappedBy = "parentId", orphanRemoval = true)
    @BatchSize(size = 50)
    private List<MawbStocksLink> mawbStocksLinkRows;
}
