package com.dpw.runner.shipment.services.entity;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.LastModifiedBy;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

@MappedSuperclass
public class TOBaseEntity implements Serializable {

    @Column
    private LocalDateTime createdAt;

    @Column
    private LocalDateTime updatedAt;


    @CreatedBy
    private String createdBy;


    @LastModifiedBy
    private String updatedBy;

    @Column
    private Boolean isActive = Boolean.TRUE;;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now(ZoneOffset.UTC);
    }
    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now(ZoneOffset.UTC);
    }
}
