package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.*;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.*;
import javax.validation.constraints.Size;
import java.util.Map;
import java.util.Objects;


@Entity
@Setter
@Getter
@Table(name = "parties")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
@SQLDelete(sql = "UPDATE parties SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
@BatchSize(size = 50)
public class Parties extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_type")
    private String entityType;

    @Column(name = "party_type")
    private String type;

    @Column(name = "org_code")
    private String orgCode;

    @Column(name = "address_code")
    @Size(max=170, message = "max limit is 170 for address_code")
    private String addressCode;

    @Column(name = "org_id")
    private String orgId;

    @Column(name = "address_id")
    private String addressId;

    @Type(type = "jsonb")
    @Column(name = "org_data", columnDefinition = "jsonb")
    private Map<String, Object> orgData;

    @Type(type = "jsonb")
    @Column(name = "address_data", columnDefinition = "jsonb")
    private Map<String, Object> addressData;

    @Column(name = "is_address_free_text")
    private Boolean isAddressFreeText;

    @Column(name = "country_code")
    @Size(max=32, message = "max limit is 170 for country_code")
    private String countryCode;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Parties that = (Parties) o;
        return Objects.equals(this.orgId, that.orgId) && Objects.equals(this.addressId, that.addressId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(orgId, addressId);
    }

}
