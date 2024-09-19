package com.dpw.runner.shipment.services.dto.TO.fzb;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class IncludedAccountingNoteFZB {

    @NotBlank(message = "Content code cannot be blank")
    @Size(max = 3, message = "Content code length must be 3 characters")
    @Pattern(regexp = "[A-Za-z]{3}", message = "Content code must contain only alphabets and be 3 characters long")
    private String contentCode;

    @NotBlank(message = "Content cannot be blank")
    @Size(max = 34, message = "Content length must be less than or equal to 34 characters")
    private String content;
}
